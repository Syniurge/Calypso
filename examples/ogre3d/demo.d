/**
 * Ogre3D D demo based on the Lighting sample. Requires Ogre3D 1.12 or later from the 1.x branch.
 *
 * Build with:
 *   (Linux)   $ ldc2 -wi -v -cpp-args "-I/usr/local/include/OGRE -I/usr/local/include/OGRE/Bites -I/usr/local/include/OGRE/Overlay -I/usr/local/include/OGRE/RTShaderSystem" -L-L/usr/local/lib -L-lOgreRTShaderSystem -L-lOgreBites -L-lOgreMain -L-lboost_system -L-lboost_thread demo.d
 *
 *   (Windows) $ ldc2.exe -wi -v -cpp-args -DBOOST_USE_WINDOWS_H -cpp-args -D_MT -cpp-args -D_DLL -cpp-args -fms-extensions -cpp-args -fdelayed-template-parsing -cpp-args -fms-compatibility -cpp-args -fms-compatibility-version=19 -cpp-args -IZ:\boost -cpp-args -IZ:\OGRE-SDK\include -cpp-args  -IZ:\OGRE-SDK\include\OGRE -cpp-args -IZ:\OGRE-SDK\include\OGRE\Overlay -cpp-args -IZ:\OGRE-SDK\include\OIS -L-lOgreOverlay -L-lOgreMain -L-lOIS -L-lboost_system -L-lboost_thread -L-LZ:\boost\lib -L-LZ:\OGRE-SDK\lib\RelWithDebInfo demo.d
 */

pragma (cppmap, "OGRE/Ogre.h");
pragma (cppmap, "OGRE/Bites/OgreApplicationContext.h");
pragma (cppmap, "OGRE/Bites/OgreCameraMan.h");
pragma (cppmap, "OGRE/Bites/OgreBitesConfigDialog.h");
pragma (cppmap, "OGRE/Bites/OgreInput.h");
pragma (cppmap, "OGRE/Bites/OgreTrays.h");

import std.stdio, std.file, std.conv;

import (C++) std._ : cppstring = string;

import (C++) Ogre._, Ogre.Math;
import (C++) Ogre.Vector3, Ogre.Radian, Ogre.ColourValue;
import (C++) Ogre.MovableObject, Ogre.Light, Ogre.Billboard,
        Ogre.BillboardSet, Ogre.RibbonTrail, Ogre.AnimationState,
        Ogre.HardwareOcclusionQuery, Ogre.Animation, Ogre.Camera,
        Ogre.Root, Ogre.RenderSystem, Ogre.MaterialManager,
        Ogre.AutoParamDataSource, Ogre.Pass, Ogre.Renderable,
        Ogre.FrameEvent, Ogre.ResourceGroupManager, Ogre.SceneManager,
        Ogre.ℂException, Ogre.LogManager, Ogre.LogMessageLevel,
        Ogre.RenderObjectListener, Ogre.Node;

import (C++) Ogre.RTShader.ShaderGenerator;

import (C++) OgreBites.ApplicationContext, OgreBites.CameraMan,
             OgreBites.CameraStyle, OgreBites.MouseMotionEvent,
             OgreBites.MouseButtonEvent, OgreBites.InputListener,
             OgreBites.TrayManager, OgreBites.TrayLocation,
             OgreBites.ApplicationContextBase, OgreBites._;

immutable ubyte cPriorityMain = 50;
immutable ubyte cPriorityQuery = 51;
immutable ubyte cPriorityLights = 55;

class DemoApplication : ApplicationContext
{
public:
    this()
    {
        super("Ogre3D + D demo");

        inputListener = new DemoInputListener;
        renderObjectListener = new DemoRenderObjectListener;
    }

protected:
    extern(C++) override void locateResources()
    {
        super.locateResources();

        ResourceGroupManager.getSingleton()
                    .addResourceLocation("resources/models", "FileSystem", "Popular");
        ResourceGroupManager.getSingleton()
                    .addResourceLocation("resources/materials/scripts", "FileSystem", "Popular");
    }

    extern(C++) override bool oneTimeConfig()
    {
        return true;
    }

    extern(C++) override void setup()
    {
        import core.stdc.stdlib : exit;

        if (this.mRoot.showConfigDialog(getNativeConfigDialog()))
            this.mRoot.initialise(false);
        else
            exit(0);

        createWindow(this.mAppName);

        locateResources();
        initialiseRTShaderSystem();
        loadResources();

        this.mRoot.addFrameListener(this);

        ApplicationContextBase.addInputListener(inputListener);

        mSceneMgr = this.mRoot.createSceneManager();

        // Register our scene with the RTSS
        ShaderGenerator.getSingletonPtr().addSceneManager(mSceneMgr);

        mTrayMgr = new TrayManager("InterfaceName", getRenderWindow());
        mTrayMgr.showFrameStats(TrayLocation.TL_BOTTOMLEFT);
        mTrayMgr.showLogo(TrayLocation.TL_BOTTOMRIGHT);
        mTrayMgr.hideCursor();

        createCamera();

        // Set our camera to orbit around the origin at a suitable distance
        mCameraMan.setStyle(CameraStyle.CS_ORBIT);
        mCameraMan.setYawPitchDist(Radian(0), Radian(0), 400);

        // and tell it to render into the main window
        getRenderWindow().addViewport(mCamera);

        // Create an ogre head and place it at the origin
        auto dlogo = mSceneMgr.createEntity("DLogo", "dlogo.mesh");
        dlogo.setRenderQueueGroup(cPriorityMain);

        auto dlogoNode = mSceneMgr.getRootSceneNode().createChildSceneNode();
        dlogoNode.setScale(1.5, 1.5, 1.5);
        dlogoNode.rotate(Vector3.UNIT_X, Radian(Math.HALF_PI));
        dlogoNode.attachObject(dlogo);

        setupLights();
    }

    void createCamera()
    {
        auto camNode = mSceneMgr.getRootSceneNode().createChildSceneNode();
        // Position it at 500 in Z direction
        camNode.setPosition(Vector3(0,0,80));
        // Look back along -Z
        camNode.lookAt(Vector3(0,0,-300), Node.TransformSpace.TS_PARENT);

        mCamera = mSceneMgr.createCamera("PlayerCam");
        mCamera.setNearClipDistance(5);
        camNode.attachObject(mCamera);

        mCameraMan = new CameraMan(camNode);
    }

    void setupLights()
    {
        mSceneMgr.setAmbientLight(ColourValue(0.1, 0.1, 0.1));  // Dim ambient lighting

        // Create a ribbon trail that our lights will leave behind
        NameValuePairList params;
        params["numberOfChains"] = "2";
        params["maxElements"] = "80";
        mTrail = cast(RibbonTrail*) mSceneMgr.createMovableObject("RibbonTrail", &params);
        mSceneMgr.getRootSceneNode().attachObject(mTrail);
        mTrail.setMaterialName("Examples/LightRibbonTrail");
        mTrail.setTrailLength(400);
        mTrail.setRenderQueueGroup(cPriorityLights);

        // Create the occlusion queries to be used in this sample
        try {
            RenderSystem* renderSystem = Root.getSingleton().getRenderSystem();
            mLight1QueryArea = renderSystem.createHardwareOcclusionQuery();
            mLight1QueryVisible = renderSystem.createHardwareOcclusionQuery();
            mLight2QueryArea = renderSystem.createHardwareOcclusionQuery();
            mLight2QueryVisible = renderSystem.createHardwareOcclusionQuery();

            mUseOcclusionQuery = (mLight1QueryArea != null) &&
                (mLight1QueryVisible != null) &&
                (mLight2QueryArea != null) &&
                (mLight2QueryVisible != null);
        }
        catch (C++) (ref ℂException e)
        {
            mUseOcclusionQuery = false;
        }

        if (mUseOcclusionQuery == false)
        {
            LogManager.getSingleton().logMessage(
                    "Sample_Lighting - Error: failed to create hardware occlusion query",
                    LogMessageLevel.LML_CRITICAL);
        }

        // Create the materials to be used by the objects used fo the occlusion query
        auto matBase = MaterialManager.getSingleton().getByName("BaseWhiteNoLighting");
        auto matQueryArea = matBase.get().clone("QueryArea");
        matQueryArea.get().setDepthWriteEnabled(false);
        matQueryArea.get().setColourWriteEnabled(false);
        matQueryArea.get().setDepthCheckEnabled(false); // Not occluded by objects
        auto matQueryVisible = matBase.get().clone("QueryVisible");
        matQueryVisible.get().setDepthWriteEnabled(false);
        matQueryVisible.get().setColourWriteEnabled(false);
        matQueryVisible.get().setDepthCheckEnabled(true); // Occluded by objects

        Light* light;
        BillboardSet* bbs;

        // Create a light node
        auto node = mSceneMgr.getRootSceneNode().createChildSceneNode(Vector3(50, 30, 0));

        // Create a 14 second animation with spline interpolation
        auto anim = mSceneMgr.createAnimation("Path1", 14);
        anim.setInterpolationMode(Animation.InterpolationMode.IM_SPLINE);

        auto track = anim.createNodeTrack(1, node);  // Create a node track for our animation

        // Enter keyframes for our track to define a path for the light to follow
        track.createNodeKeyFrame(0).setTranslate(Vector3(50, 30, -40));
        track.createNodeKeyFrame(2).setTranslate(Vector3(100, -30, -40));
        track.createNodeKeyFrame(4).setTranslate(Vector3(120, -80, 110));
        track.createNodeKeyFrame(6).setTranslate(Vector3(30, -80, 10));
        track.createNodeKeyFrame(8).setTranslate(Vector3(-50, 30, -90));
        track.createNodeKeyFrame(10).setTranslate(Vector3(-150, -20, -140));
        track.createNodeKeyFrame(12).setTranslate(Vector3(-50, -30, -40));
        track.createNodeKeyFrame(14).setTranslate(Vector3(50, 30, -40));

        // Create an animation state from the animation and enable it
        mYellowLightAnimState = mSceneMgr.createAnimationState("Path1");
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
        bbs.setMaterialName("Examples/Flare");
        bbs.setRenderQueueGroup(cPriorityLights);
        node.attachObject(bbs);

        if (mUseOcclusionQuery)
        {
            // Attach a billboard which will be used to get a relative area occupied by the light
            mLight1BBQueryArea = mSceneMgr.createBillboardSet(1);
            mLight1BBQueryArea.setDefaultDimensions(10,10);
            mLight1BBQueryArea.createBillboard(Vector3.ZERO);
            mLight1BBQueryArea.setMaterialName("QueryArea");
            mLight1BBQueryArea.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight1BBQueryArea);

            // Attach a billboard which will be used to get the visible area occupied by the light
            mLight1BBQueryVisible = mSceneMgr.createBillboardSet(1);
            mLight1BBQueryVisible.setDefaultDimensions(10,10);
            mLight1BBQueryVisible.createBillboard(Vector3.ZERO);
            mLight1BBQueryVisible.setMaterialName("QueryVisible");
            mLight1BBQueryVisible.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight1BBQueryVisible);
        }

        // Create a second light node
        auto vec3_rootPos = Vector3(-50, 100, 0);
        node = mSceneMgr.getRootSceneNode().createChildSceneNode(vec3_rootPos);

        // Create a 10 second animation with spline interpolation
        anim = mSceneMgr.createAnimation("Path2", 10);
        anim.setInterpolationMode(Animation.InterpolationMode.IM_SPLINE);

        track = anim.createNodeTrack(1, node);  // Create a node track for our animation

        // Enter keyframes for our track to define a path for the light to follow
        track.createNodeKeyFrame(0).setTranslate(Vector3(-50, 100, 0));
        track.createNodeKeyFrame(2).setTranslate(Vector3(-100, 150, -30));
        track.createNodeKeyFrame(4).setTranslate(Vector3(-200, 0, 40));
        track.createNodeKeyFrame(6).setTranslate(Vector3(0, -150, 70));
        track.createNodeKeyFrame(8).setTranslate(Vector3(50, 0, 30));
        track.createNodeKeyFrame(10).setTranslate(Vector3(-50, 100, 0));

        // Create an animation state from the animation and enable it
        mGreenLightAnimState = mSceneMgr.createAnimationState("Path2");
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
        bbs.setMaterialName("Examples/Flare");
        bbs.setRenderQueueGroup(cPriorityLights);
        node.attachObject(bbs);

        if (mUseOcclusionQuery)
        {
            // Attach a billboard which will be used to get a relative area occupied by the light
            mLight2BBQueryArea = mSceneMgr.createBillboardSet(1);
            mLight2BBQueryArea.setDefaultDimensions(10,10);
            mLight2BBQueryArea.createBillboard(Vector3.ZERO);
            mLight2BBQueryArea.setMaterialName("QueryArea");
            mLight2BBQueryArea.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight2BBQueryArea);

            // Attach a billboard which will be used to get the visible area occupied by the light
            mLight2BBQueryVisible = mSceneMgr.createBillboardSet(1);
            mLight2BBQueryVisible.setDefaultDimensions(10,10);
            mLight2BBQueryVisible.createBillboard(Vector3.ZERO);
            mLight2BBQueryVisible.setMaterialName("QueryVisible");
            mLight2BBQueryVisible.setRenderQueueGroup(cPriorityQuery);
            node.attachObject(mLight2BBQueryVisible);
        }

        // Setup the listener for the occlusion query
        if (mUseOcclusionQuery)
        {
            mSceneMgr.addRenderObjectListener(renderObjectListener);
            mDoOcclusionQuery = true;
        }
    }

    extern(C++) override bool frameRenderingQueued(scope ref const(FrameEvent) evt)
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

    extern(C++) override void shutdown()
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

        super.shutdown();
    }

    // Alternative to multiple inheritance: use nested classes
    class DemoRenderObjectListener : RenderObjectListener
    {
        // Event raised when render single object started.
        extern(C++) override void notifyRenderSingleObject(Renderable* rend,
                        const(Pass)* pass, const(AutoParamDataSource)* source,
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
    }

    class DemoInputListener : InputListener
    {
    extern(C++):
        override bool mouseMoved(ref scope const(MouseMotionEvent) evt)
        {
            if (mTrayMgr.mouseMoved(evt))
                return true;

            mCameraMan.mouseMoved(evt);
            return true;
        }

        override bool mousePressed(ref scope const(MouseButtonEvent) evt)
        {
            if (mTrayMgr.mousePressed(evt))
                return true;

            mCameraMan.mousePressed(evt);
            return true;
        }

        override bool mouseReleased(ref scope const(MouseButtonEvent) evt)
        {
            if (mTrayMgr.mouseReleased(evt))
                return true;

            mCameraMan.mouseReleased(evt);
            return true;
        }
    }

    DemoInputListener inputListener;
    DemoRenderObjectListener renderObjectListener;

    SceneManager* mSceneMgr;
    Camera*       mCamera;
    CameraMan*    mCameraMan;
    TrayManager*  mTrayMgr;

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
    app.initApp();
    app.getRoot().startRendering();
    app.closeApp();
}
