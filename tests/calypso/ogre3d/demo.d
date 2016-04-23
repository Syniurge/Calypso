/**
 * Ogre3D D demo based on the Lighting sample and the wiki tutorial framework.
 *
 * Build with:
 *   $ clang++ -I/usr/local/include/OGRE -I/usr/local/include/OGRE/Overlay -I/usr/include/OIS -c BaseApplication.cpp -o BaseApplication.cpp.o
 *   $ ldc2 -wi -v -cpp-args -I/usr/local/include/OGRE -cpp-args -I/usr/local/include/OGRE/Overlay -cpp-args -I/usr/include/OIS -LBaseApplication.cpp.o -L-lOgreOverlay -L-lOgreMain -L-lOIS -L-lboost_system -L-lboost_thread -L-lstdc++ demo.d
 */

modmap (C++) "OGRE/Ogre.h";
modmap (C++) "OGRE/InputContext.h";
modmap (C++) "OGRE/SdkCameraMan.h";
modmap (C++) "BaseApplication.h";

import std.stdio, std.file, std.conv;

import (C++) std._ : cppstring = string;

import (C++) Ogre._, Ogre.Math;
import (C++) Ogre.Vector3, Ogre.Radian, Ogre.ColourValue;
import (C++) Ogre.MovableObject, Ogre.Light, Ogre.Billboard,
        Ogre.BillboardSet, Ogre.RibbonTrail, Ogre.AnimationState,
        Ogre.HardwareOcclusionQuery, Ogre.Animation,
        Ogre.Root, Ogre.RenderSystem, Ogre.MaterialManager,
        Ogre.AutoParamDataSource, Ogre.Pass, Ogre.Renderable,
        Ogre.FrameEvent, Ogre.ResourceGroupManager;
import (C++) OgreBites.SdkCameraMan, OgreBites.CameraStyle;

import (C++) BaseApplication;

template cppstr(string str)
{
    const char[] cppstr = "auto s_" ~ str ~ " = cppstring(\"" ~ str ~ "\");";
}

immutable ubyte cPriorityMain = 50;
immutable ubyte cPriorityQuery = 51;
immutable ubyte cPriorityLights = 55;

class DemoApplication : BaseApplication
{
public:
    this()
    {
        super();

        if (exists("/usr/local/share/OGRE/plugins.cfg"))
            m_ResourcePath = "/usr/local/share/OGRE/";
        else
            m_ResourcePath = "/usr/share/OGRE/";
        assert(exists(to!string(m_ResourcePath.c_str)));
    }

//   ~this() {}

protected:
    extern(C++) override void setupResources()
    {
        super.setupResources();

        mixin(cppstr!"FileSystem");
        mixin(cppstr!"Popular");
        auto s_resModels = cppstring("resources/models");
        auto s_resMatScripts = cppstring("resources/materials/scripts");

        ResourceGroupManager.getSingleton().addResourceLocation(s_resModels, s_FileSystem, s_Popular);
        ResourceGroupManager.getSingleton().addResourceLocation(s_resMatScripts, s_FileSystem, s_Popular);
    }

    extern(C++) override void createScene()
    {
        // Set our camera to orbit around the origin at a suitable distance
        mCameraMan.setStyle(CameraStyle.CS_ORBIT);
        mCameraMan.setYawPitchDist(Radian(0), Radian(0), 400);

        auto s_entName = cppstring("DLogo");
        auto s_entModelName = cppstring("dlogo.mesh");

        auto rad_halfPi = Radian(Math.HALF_PI);

        // Create an ogre head and place it at the origin
        auto dlogo = mSceneMgr.createEntity(s_entName, s_entModelName);
        dlogo.setRenderQueueGroup(cPriorityMain);

        auto dlogoNode = mSceneMgr.getRootSceneNode().createChildSceneNode();
        dlogoNode.setScale(1.5, 1.5, 1.5);
        dlogoNode.rotate(Vector3.UNIT_X, rad_halfPi);
        dlogoNode.attachObject(dlogo);

        setupLights();
    }

    void setupLights()
    {
        auto ambLightCol = ColourValue(0.1, 0.1, 0.1);
        mSceneMgr.setAmbientLight(ambLightCol);  // Dim ambient lighting

        mixin(cppstr!"numberOfChains");
        mixin(cppstr!"maxElements");
        mixin(cppstr!"RibbonTrail");
        auto s_ribbonMatName = cppstring("Examples/LightRibbonTrail");

        // Create a ribbon trail that our lights will leave behind
        NameValuePairList params;
        params[s_numberOfChains] = "2";
        params[s_maxElements] = "80";
        mTrail = cast(RibbonTrail*) cast(void*) mSceneMgr.createMovableObject(s_RibbonTrail, &params); // TODO implement dynamic casts
        mSceneMgr.getRootSceneNode().attachObject(mTrail);
        mTrail.setMaterialName(s_ribbonMatName);
        mTrail.setTrailLength(400);
        mTrail.setRenderQueueGroup(cPriorityLights);

        // Create the occlusion queries to be used in this sample
//         try {
            RenderSystem* renderSystem = Root.getSingleton().getRenderSystem();
            mLight1QueryArea = renderSystem.createHardwareOcclusionQuery();
            mLight1QueryVisible = renderSystem.createHardwareOcclusionQuery();
            mLight2QueryArea = renderSystem.createHardwareOcclusionQuery();
            mLight2QueryVisible = renderSystem.createHardwareOcclusionQuery();

            mUseOcclusionQuery = (mLight1QueryArea != null) &&
                (mLight1QueryVisible != null) &&
                (mLight2QueryArea != null) &&
                (mLight2QueryVisible != null);
//         }
//         catch (Ogre.Exception e)
//         {
//             mUseOcclusionQuery = false;
//         }
//         if (mUseOcclusionQuery == false)
//         {
//             LogManager.getSingleton().logMessage("Sample_Lighting - Error: failed to create hardware occlusion query", LML_CRITICAL);
//         }

        assert(mUseOcclusionQuery);

        mixin(cppstr!"BaseWhiteNoLighting");
        mixin(cppstr!"QueryArea");
        mixin(cppstr!"QueryVisible");

        // Create the materials to be used by the objects used fo the occlusion query
        auto matBase = MaterialManager.getSingleton().getByName(s_BaseWhiteNoLighting);
        auto matQueryArea = matBase.get().clone(s_QueryArea);
        matQueryArea.get().setDepthWriteEnabled(false);
        matQueryArea.get().setColourWriteEnabled(false);
        matQueryArea.get().setDepthCheckEnabled(false); // Not occluded by objects
        auto matQueryVisible = matBase.get().clone(s_QueryVisible);
        matQueryVisible.get().setDepthWriteEnabled(false);
        matQueryVisible.get().setColourWriteEnabled(false);
        matQueryVisible.get().setDepthCheckEnabled(true); // Occluded by objects

        Light* light;
        BillboardSet* bbs;

        // Create a light node
        auto vec3_nodePos = Vector3(50, 30, 0);
        auto node = mSceneMgr.getRootSceneNode().createChildSceneNode(vec3_nodePos);

        mixin(cppstr!"Path1");
        mixin(cppstr!"Path2");

        // Create a 14 second animation with spline interpolation
        auto anim = mSceneMgr.createAnimation(s_Path1, 14);
        anim.setInterpolationMode(Animation.InterpolationMode.IM_SPLINE);

        auto track = anim.createNodeTrack(1, node);  // Create a node track for our animation

        auto vec3_kf0tr = Vector3(50, 30, -40);
        auto vec3_kf2tr = Vector3(100, -30, -40);
        auto vec3_kf4tr = Vector3(120, -80, 110);
        auto vec3_kf6tr = Vector3(30, -80, 10);
        auto vec3_kf8tr = Vector3(-50, 30, -90);
        auto vec3_kf10tr = Vector3(-150, -20, -140);
        auto vec3_kf12tr = Vector3(-50, -30, -40);
        auto vec3_kf14tr = Vector3(50, 30, -40);

        // Enter keyframes for our track to define a path for the light to follow
        track.createNodeKeyFrame(0).setTranslate(vec3_kf0tr);
        track.createNodeKeyFrame(2).setTranslate(vec3_kf2tr);
        track.createNodeKeyFrame(4).setTranslate(vec3_kf4tr);
        track.createNodeKeyFrame(6).setTranslate(vec3_kf6tr);
        track.createNodeKeyFrame(8).setTranslate(vec3_kf8tr);
        track.createNodeKeyFrame(10).setTranslate(vec3_kf10tr);
        track.createNodeKeyFrame(12).setTranslate(vec3_kf12tr);
        track.createNodeKeyFrame(14).setTranslate(vec3_kf14tr);

        // Create an animation state from the animation and enable it
        mYellowLightAnimState = mSceneMgr.createAnimationState(s_Path1);
        mYellowLightAnimState.setEnabled(true);

        // Set initial settings for the ribbon mTrail and add the light node
        mTrail.setInitialColour(0, 1.0, 0.9, 0.95);
        mTrail.setColourChange(0, 0.5, 0.5, 0.5, 0.5);
        mTrail.setInitialWidth(0, 5);
        mTrail.addNode(node);


        // Attach a light with the same colour to the light node
        light = mSceneMgr.createLight();
        light.setDiffuseColour(mTrail.getInitialColour(0));
        node.attachObject(light);

        // Attach a flare with the same colour to the light node
        bbs = mSceneMgr.createBillboardSet(1);
        mLight1BBFlare = bbs.createBillboard(Vector3.ZERO, mTrail.getInitialColour(0));
        auto s_ExFlare = cppstring("Examples/Flare");
        bbs.setMaterialName(s_ExFlare);
        bbs.setRenderQueueGroup(cPriorityLights);
        node.attachObject(bbs);

        if (mUseOcclusionQuery)
        {
            // Attach a billboard which will be used to get a relative area occupied by the light
            mLight1BBQueryArea = mSceneMgr.createBillboardSet(1);
            mLight1BBQueryArea.setDefaultDimensions(10,10);
            mLight1BBQueryArea.createBillboard(Vector3.ZERO);
            mLight1BBQueryArea.setMaterialName(s_QueryArea);
            mLight1BBQueryArea.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight1BBQueryArea);

            // Attach a billboard which will be used to get the visible area occupied by the light
            mLight1BBQueryVisible = mSceneMgr.createBillboardSet(1);
            mLight1BBQueryVisible.setDefaultDimensions(10,10);
            mLight1BBQueryVisible.createBillboard(Vector3.ZERO);
            mLight1BBQueryVisible.setMaterialName(s_QueryVisible);
            mLight1BBQueryVisible.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight1BBQueryVisible);
        }

        // Create a second light node
        auto vec3_rootPos = Vector3(-50, 100, 0);
        node = mSceneMgr.getRootSceneNode().createChildSceneNode(vec3_rootPos);

        // Create a 10 second animation with spline interpolation
        anim = mSceneMgr.createAnimation(s_Path2, 10);
        anim.setInterpolationMode(Animation.InterpolationMode.IM_SPLINE);

        track = anim.createNodeTrack(1, node);  // Create a node track for our animation

        auto vec3_trackkf0tr = Vector3(-50, 100, 0);
        auto vec3_trackkf2tr = Vector3(-100, 150, -30);
        auto vec3_trackkf4tr = Vector3(-200, 0, 40);
        auto vec3_trackkf6tr = Vector3(0, -150, 70);
        auto vec3_trackkf8tr = Vector3(50, 0, 30);
        auto vec3_trackkf10tr = Vector3(-50, 100, 0);

        // Enter keyframes for our track to define a path for the light to follow
        track.createNodeKeyFrame(0).setTranslate(vec3_trackkf0tr);
        track.createNodeKeyFrame(2).setTranslate(vec3_trackkf2tr);
        track.createNodeKeyFrame(4).setTranslate(vec3_trackkf4tr);
        track.createNodeKeyFrame(6).setTranslate(vec3_trackkf6tr);
        track.createNodeKeyFrame(8).setTranslate(vec3_trackkf8tr);
        track.createNodeKeyFrame(10).setTranslate(vec3_trackkf10tr);

        // Create an animation state from the animation and enable it
        mGreenLightAnimState = mSceneMgr.createAnimationState(s_Path2);
        mGreenLightAnimState.setEnabled(true);

        // Set initial settings for the ribbon mTrail and add the light node
        mTrail.setInitialColour(1, 1.0, 0.35, 0.35);
        mTrail.setColourChange(1, 0.5, 0.5, 0.5, 0.5);
        mTrail.setInitialWidth(1, 5);
        mTrail.addNode(node);

        // Attach a light with the same colour to the light node
        light = mSceneMgr.createLight();
        light.setDiffuseColour(mTrail.getInitialColour(1));
        node.attachObject(light);

        // Attach a flare with the same colour to the light node
        bbs = mSceneMgr.createBillboardSet(1);
        mLight2BBFlare = bbs.createBillboard(Vector3.ZERO, mTrail.getInitialColour(1));
        bbs.setMaterialName(s_ExFlare);
        bbs.setRenderQueueGroup(cPriorityLights);
        node.attachObject(bbs);

        if (mUseOcclusionQuery)
        {
            // Attach a billboard which will be used to get a relative area occupied by the light
            mLight2BBQueryArea = mSceneMgr.createBillboardSet(1);
            mLight2BBQueryArea.setDefaultDimensions(10,10);
            mLight2BBQueryArea.createBillboard(Vector3.ZERO);
            mLight2BBQueryArea.setMaterialName(s_QueryArea);
            mLight2BBQueryArea.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight2BBQueryArea);

            // Attach a billboard which will be used to get the visible area occupied by the light
            mLight2BBQueryVisible = mSceneMgr.createBillboardSet(1);
            mLight2BBQueryVisible.setDefaultDimensions(10,10);
            mLight2BBQueryVisible.createBillboard(Vector3.ZERO);
            mLight2BBQueryVisible.setMaterialName(s_QueryVisible);
            mLight2BBQueryVisible.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight2BBQueryVisible);
        }

        // Setup the listener for the occlusion query
        if (mUseOcclusionQuery)
        {
            mSceneMgr.addRenderObjectListener(this);
            mDoOcclusionQuery = true;
        }
    }

    extern(C++) override bool frameRenderingQueued(ref const(FrameEvent) evt)
    {
        // Move the lights along their paths
        mGreenLightAnimState.addTime(evt.timeSinceLastFrame);
        mYellowLightAnimState.addTime(evt.timeSinceLastFrame);

        // Modulate the light flare according to performed occlusion queries
        if (mUseOcclusionQuery)
        {
            // Stop occlusion queries until we get their information
            // (may not happen on the same frame they are requested in)
            mDoOcclusionQuery = false;

            // Check if all query information available
            if ((mLight1QueryArea.isStillOutstanding() == false) &&
                (mLight1QueryVisible.isStillOutstanding() == false) &&
                (mLight2QueryArea.isStillOutstanding() == false) &&
                (mLight2QueryVisible.isStillOutstanding() == false))
            {
                // Modulate the lights according to the query data
                uint lightAreaCount;
                uint lightVisibleCount;
                float ratio;

                mLight1QueryArea.pullOcclusionQuery(&lightAreaCount);
                mLight1QueryVisible.pullOcclusionQuery(&lightVisibleCount);
                ratio = (cast(float) lightVisibleCount) / (cast(float) lightAreaCount);
                auto c_flare0 = mTrail.getInitialColour(0) * ratio;
                mLight1BBFlare.setColour(c_flare0);

                mLight2QueryArea.pullOcclusionQuery(&lightAreaCount);
                mLight2QueryVisible.pullOcclusionQuery(&lightVisibleCount);
                ratio = (cast(float) lightVisibleCount) / (cast(float) lightAreaCount);
                auto c_flare1 = mTrail.getInitialColour(1) * ratio;
                mLight2BBFlare.setColour(c_flare1);

                // Request new query data
                mDoOcclusionQuery = true;
            }
        }
        return super.frameRenderingQueued(evt);   // don't forget the parent class updates!
    }

    // Event raised when render single object started.
    extern(C++) override void notifyRenderSingleObject(Renderable* rend, const(Pass)* pass, const(AutoParamDataSource)* source,
            const(LightList)* pLightList, bool suppressRenderStateChanges)
    {
        //
        // The following code activates and deactivates the occlusion queries
        // so that the queries only include the rendering of their intended targets
        //

        // Close the last occlusion query
        // Each occlusion query should only last a single rendering
        if (mActiveQuery != null)
        {
            mActiveQuery.endOcclusionQuery();
            mActiveQuery = null;
        }

        // Open a new occlusion query
        if (mDoOcclusionQuery == true)
        {
            // Check if a the object being rendered needs
            // to be occlusion queried, and by which query instance.
            if (rend == mLight1BBQueryArea)
                mActiveQuery = mLight1QueryArea;
            else if (rend == mLight1BBQueryVisible)
                mActiveQuery = mLight1QueryVisible;
            else if (rend == mLight2BBQueryArea)
                mActiveQuery = mLight2QueryArea;
            else if (rend == mLight2BBQueryVisible)
                mActiveQuery = mLight2QueryVisible;

            if (mActiveQuery != null)
            {
                mActiveQuery.beginOcclusionQuery();
            }
        }
    }

    void cleanupContent()
    {
        auto renderSystem = Root.getSingleton().getRenderSystem();
        if (mLight1QueryArea != null)
            renderSystem.destroyHardwareOcclusionQuery(mLight1QueryArea);
        if (mLight1QueryVisible != null)
            renderSystem.destroyHardwareOcclusionQuery(mLight1QueryVisible);
        if (mLight2QueryArea != null)
            renderSystem.destroyHardwareOcclusionQuery(mLight2QueryArea);
        if (mLight2QueryVisible != null)
            renderSystem.destroyHardwareOcclusionQuery(mLight2QueryVisible);
    }

    AnimationState* mGreenLightAnimState;
    AnimationState* mYellowLightAnimState;

    RibbonTrail* mTrail;

    Billboard* mLight1BBFlare;
    BillboardSet* mLight1BBQueryArea;
    BillboardSet* mLight1BBQueryVisible;
    Billboard* mLight2BBFlare;
    BillboardSet* mLight2BBQueryArea;
    BillboardSet* mLight2BBQueryVisible;

    HardwareOcclusionQuery* mLight1QueryArea;
    HardwareOcclusionQuery* mLight1QueryVisible;
    HardwareOcclusionQuery* mLight2QueryArea;
    HardwareOcclusionQuery* mLight2QueryVisible;
    HardwareOcclusionQuery* mActiveQuery;

    bool mUseOcclusionQuery = false;
    bool mDoOcclusionQuery;
}

void main()
{
    auto app = new DemoApplication;
    app.go();
}
