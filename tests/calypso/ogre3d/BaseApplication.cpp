/*
-----------------------------------------------------------------------------
Filename:    BaseApplication.cpp
-----------------------------------------------------------------------------

This source file is part of the
   ___                 __    __ _ _    _
  /___\__ _ _ __ ___  / / /\ \ (_) | _(_)
 //  // _` | '__/ _ \ \ \/  \/ / | |/ / |
/ \_// (_| | | |  __/  \  /\  /| |   <| |
\___/ \__, |_|  \___|   \/  \/ |_|_|\_\_|
      |___/
Tutorial Framework (for Ogre 1.9 and 1.10)
http://www.ogre3d.org/wiki/
-----------------------------------------------------------------------------
*/

#include "BaseApplication.h"

#include <OgreGpuProgramManager.h>

#if OGRE_PLATFORM == OGRE_PLATFORM_APPLE
#include <macUtils.h>
#endif

#if OGRE_VERSION_MINOR == 10 // Ogre 1.10
using Ogre::BLANKSTRING;
#else // Ogre 1.9
const Ogre::String &BLANKSTRING = Ogre::StringUtil::BLANK;
#endif

//---------------------------------------------------------------------------
BaseApplication::BaseApplication(void)
    : mRoot(0),
    mCamera(0),
    mSceneMgr(0),
    mWindow(0),
    mResourcesCfg(BLANKSTRING),
    mPluginsCfg(BLANKSTRING),
    mTrayMgr(0),
    mCameraMan(0),
    mDetailsPanel(0),
    mCursorWasVisible(false),
    mShutDown(false),
    mInputManager(0),
    mMouse(0),
    mKeyboard(0),
    mOverlaySystem(0)
{
#if OGRE_PLATFORM == OGRE_PLATFORM_APPLE
    m_ResourcePath = Ogre::macBundlePath() + "/Contents/Resources/";
#else
    m_ResourcePath = "";
#endif
}

//---------------------------------------------------------------------------
BaseApplication::~BaseApplication(void)
{
    if (mTrayMgr) delete mTrayMgr;
    if (mCameraMan) delete mCameraMan;
    if (mOverlaySystem) delete mOverlaySystem;

    // Remove ourself as a Window listener
    Ogre::WindowEventUtilities::removeWindowEventListener(mWindow, this);
    windowClosed(mWindow);
    delete mRoot;
}

//---------------------------------------------------------------------------
bool BaseApplication::configure(void)
{
    // Show the configuration dialog and initialise the system.
    // You can skip this and use root.restoreConfig() to load configuration
    // settings if you were sure there are valid ones saved in ogre.cfg.
    if(mRoot->showConfigDialog())
    {
        // If returned true, user clicked OK so initialise.
        // Here we choose to let the system create a default rendering window by passing 'true'.
        mWindow = mRoot->initialise(true, "Ogre3D + D demo");

        return true;
    }
    else
    {
        return false;
    }
}
//---------------------------------------------------------------------------
void BaseApplication::chooseSceneManager(void)
{
    // Get the SceneManager, in this case a generic one
    mSceneMgr = mRoot->createSceneManager(Ogre::ST_GENERIC);

    // Initialize the OverlaySystem (changed for Ogre 1.9)
    mOverlaySystem = new Ogre::OverlaySystem();
    mSceneMgr->addRenderQueueListener(mOverlaySystem);
}
//---------------------------------------------------------------------------
void BaseApplication::createCamera(void)
{
    // Create the camera
    mCamera = mSceneMgr->createCamera("PlayerCam");

    // Position it at 500 in Z direction
    mCamera->setPosition(Ogre::Vector3(0,0,80));
    // Look back along -Z
    mCamera->lookAt(Ogre::Vector3(0,0,-300));
    mCamera->setNearClipDistance(5);

    mCameraMan = new OgreBites::SdkCameraMan(mCamera);   // Create a default camera controller
}
//---------------------------------------------------------------------------
void BaseApplication::createFrameListener(void)
{
    Ogre::LogManager::getSingletonPtr()->logMessage("*** Initializing OIS ***");
    OIS::ParamList pl;
    size_t windowHnd = 0;
    std::ostringstream windowHndStr;

    mWindow->getCustomAttribute("WINDOW", &windowHnd);
    windowHndStr << windowHnd;
    pl.insert(std::make_pair(std::string("WINDOW"), windowHndStr.str()));

#if defined OIS_WIN32_PLATFORM
    pl.insert(std::make_pair(std::string("w32_mouse"), std::string("DISCL_FOREGROUND" )));
    pl.insert(std::make_pair(std::string("w32_mouse"), std::string("DISCL_NONEXCLUSIVE")));
    pl.insert(std::make_pair(std::string("w32_keyboard"), std::string("DISCL_FOREGROUND")));
    pl.insert(std::make_pair(std::string("w32_keyboard"), std::string("DISCL_NONEXCLUSIVE")));
#elif defined OIS_LINUX_PLATFORM
    pl.insert(std::make_pair(std::string("x11_mouse_grab"), std::string("false")));
    pl.insert(std::make_pair(std::string("x11_mouse_hide"), std::string("false")));
    pl.insert(std::make_pair(std::string("x11_keyboard_grab"), std::string("false")));
    pl.insert(std::make_pair(std::string("XAutoRepeatOn"), std::string("true")));
#endif

    mInputManager = OIS::InputManager::createInputSystem(pl);

    mKeyboard = static_cast<OIS::Keyboard*>(mInputManager->createInputObject(OIS::OISKeyboard, true));
    mMouse = static_cast<OIS::Mouse*>(mInputManager->createInputObject(OIS::OISMouse, true));

    mMouse->setEventCallback(this);
    mKeyboard->setEventCallback(this);

    // Set initial mouse clipping size
    windowResized(mWindow);

    // Register as a Window listener
    Ogre::WindowEventUtilities::addWindowEventListener(mWindow, this);

    mInputContext.mKeyboard = mKeyboard;
    mInputContext.mMouse = mMouse;
    mTrayMgr = new OgreBites::SdkTrayManager("InterfaceName", mWindow, mInputContext, this);
    mTrayMgr->showFrameStats(OgreBites::TL_BOTTOMLEFT);
    mTrayMgr->showLogo(OgreBites::TL_BOTTOMRIGHT);
    mTrayMgr->hideCursor();

    // Create a params panel for displaying sample details
    Ogre::StringVector items;
    items.push_back("cam.pX");
    items.push_back("cam.pY");
    items.push_back("cam.pZ");
    items.push_back("");
    items.push_back("cam.oW");
    items.push_back("cam.oX");
    items.push_back("cam.oY");
    items.push_back("cam.oZ");
    items.push_back("");
    items.push_back("Filtering");
    items.push_back("Poly Mode");

    mDetailsPanel = mTrayMgr->createParamsPanel(OgreBites::TL_NONE, "DetailsPanel", 200, items);
    mDetailsPanel->setParamValue(9, "Bilinear");
    mDetailsPanel->setParamValue(10, "Solid");
    mDetailsPanel->hide();

    mRoot->addFrameListener(this);
}
//---------------------------------------------------------------------------
void BaseApplication::destroyScene(void)
{
}
//---------------------------------------------------------------------------
void BaseApplication::createViewports(void)
{
    // Create one viewport, entire window
    Ogre::Viewport* vp = mWindow->addViewport(mCamera);
    vp->setBackgroundColour(Ogre::ColourValue(0,0,0));

    // Alter the camera aspect ratio to match the viewport
    mCamera->setAspectRatio(Ogre::Real(vp->getActualWidth()) / Ogre::Real(vp->getActualHeight()));
}
//---------------------------------------------------------------------------
void BaseApplication::setupResources(void)
{
    // Load resource paths from config file
    Ogre::ConfigFile cf;
    cf.load(mResourcesCfg);

    // Go through all sections & settings in the file
    Ogre::ConfigFile::SectionIterator seci = cf.getSectionIterator();

    Ogre::String secName, typeName, archName;
    while (seci.hasMoreElements())
    {
        secName = seci.peekNextKey();
        Ogre::ConfigFile::SettingsMultiMap *settings = seci.getNext();
        Ogre::ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i)
        {
            typeName = i->first;
            archName = i->second;

#if OGRE_PLATFORM == OGRE_PLATFORM_APPLE
            // OS X does not set the working directory relative to the app.
            // In order to make things portable on OS X we need to provide
            // the loading with it's own bundle path location.
            if (!Ogre::StringUtil::startsWith(archName, "/", false)) // only adjust relative directories
                archName = Ogre::String(Ogre::macBundlePath() + "/" + archName);
#endif

            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(
                archName, typeName, secName);
        }
    }

    const Ogre::ResourceGroupManager::LocationList& genLocs = Ogre::ResourceGroupManager::getSingleton().getResourceLocationList("General");
    archName = genLocs.front()->archive->getName();
#       if OGRE_PLATFORM == OGRE_PLATFORM_APPLE
    archName = Ogre::macBundlePath() + "/Contents/Resources/Media";
#       else
    archName = Ogre::StringUtil::replaceAll(archName, "Media/../../Tests/Media", "");
    archName = Ogre::StringUtil::replaceAll(archName, "media/../../Tests/Media", "");
#       endif
    typeName = "FileSystem";
    secName = "Popular";

#       ifdef OGRE_BUILD_PLUGIN_CG
    bool use_HLSL_Cg_shared = true;
#       else
    bool use_HLSL_Cg_shared = Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("hlsl");
#       endif

    // Add locations for supported shader languages
    if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsles"))
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/GLSLES", typeName, secName);
    }
    else if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsl"))
    {
        if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsl150"))
        {
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/GLSL150", typeName, secName);
        }
        else
        {
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/GLSL", typeName, secName);
        }

        if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsl400"))
        {
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/GLSL400", typeName, secName);
        }
    }
    else if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("hlsl"))
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/HLSL", typeName, secName);
    }
#       ifdef OGRE_BUILD_PLUGIN_CG
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/Cg", typeName, secName);
#       endif
    if (use_HLSL_Cg_shared)
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/materials/programs/HLSL_Cg", typeName, secName);
    }

#       ifdef INCLUDE_RTSHADER_SYSTEM
    if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsles"))
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/GLSLES", typeName, secName);
    }
    else if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsl"))
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/GLSL", typeName, secName);
        if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("glsl150"))
        {
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/GLSL150", typeName, secName);
        }
    }
    else if(Ogre::GpuProgramManager::getSingleton().isSyntaxSupported("hlsl"))
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/HLSL", typeName, secName);
    }
#           ifdef OGRE_BUILD_PLUGIN_CG
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/Cg", typeName, secName);
#           endif
    if (use_HLSL_Cg_shared)
    {
        Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName + "/RTShaderLib/HLSL_Cg", typeName, secName);
    }
#       endif /* INCLUDE_RTSHADER_SYSTEM */
}
//---------------------------------------------------------------------------
void BaseApplication::createResourceListener(void)
{
}
//---------------------------------------------------------------------------
void BaseApplication::loadResources(void)
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}
//---------------------------------------------------------------------------
void BaseApplication::go(void)
{
#ifdef _DEBUG
#ifndef OGRE_STATIC_LIB
    mResourcesCfg = m_ResourcePath + "resources_d.cfg";
    mPluginsCfg = m_ResourcePath + "plugins_d.cfg";
#else
    mResourcesCfg = "resources_d.cfg";
    mPluginsCfg = "plugins_d.cfg";
#endif
#else
#ifndef OGRE_STATIC_LIB
    mResourcesCfg = m_ResourcePath + "resources.cfg";
    mPluginsCfg = m_ResourcePath + "plugins.cfg";
#else
    mResourcesCfg = "resources.cfg";
    mPluginsCfg = "plugins.cfg";
#endif
#endif

    if (!setup())
        return;

    mRoot->startRendering();

    // Clean up
    destroyScene();
}
//---------------------------------------------------------------------------
bool BaseApplication::setup(void)
{
    mRoot = new Ogre::Root(mPluginsCfg);

    bool carryOn = configure();
    if (!carryOn) return false;

    setupResources();

    chooseSceneManager();
    createCamera();
    createViewports();

    // Set default mipmap level (NB some APIs ignore this)
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);

    // Create any resource listeners (for loading screens)
    createResourceListener();
    // Load resources
    loadResources();

    // Create the scene
    createScene();

    createFrameListener();

    return true;
};
//---------------------------------------------------------------------------
bool BaseApplication::frameRenderingQueued(const Ogre::FrameEvent& evt)
{
    if(mWindow->isClosed())
        return false;

    if(mShutDown)
        return false;

    // Need to capture/update each device
    mKeyboard->capture();
    mMouse->capture();

    mTrayMgr->frameRenderingQueued(evt);

    if (!mTrayMgr->isDialogVisible())
    {
        mCameraMan->frameRenderingQueued(evt);   // If dialog isn't up, then update the camera
        if (mDetailsPanel->isVisible())          // If details panel is visible, then update its contents
        {
            mDetailsPanel->setParamValue(0, Ogre::StringConverter::toString(mCamera->getDerivedPosition().x));
            mDetailsPanel->setParamValue(1, Ogre::StringConverter::toString(mCamera->getDerivedPosition().y));
            mDetailsPanel->setParamValue(2, Ogre::StringConverter::toString(mCamera->getDerivedPosition().z));
            mDetailsPanel->setParamValue(4, Ogre::StringConverter::toString(mCamera->getDerivedOrientation().w));
            mDetailsPanel->setParamValue(5, Ogre::StringConverter::toString(mCamera->getDerivedOrientation().x));
            mDetailsPanel->setParamValue(6, Ogre::StringConverter::toString(mCamera->getDerivedOrientation().y));
            mDetailsPanel->setParamValue(7, Ogre::StringConverter::toString(mCamera->getDerivedOrientation().z));
        }
    }

    return true;
}
//---------------------------------------------------------------------------
bool BaseApplication::keyPressed( const OIS::KeyEvent &arg )
{
    if (mTrayMgr->isDialogVisible()) return true;   // don't process any more keys if dialog is up

    if (arg.key == OIS::KC_F)   // toggle visibility of advanced frame stats
    {
        mTrayMgr->toggleAdvancedFrameStats();
    }
    else if (arg.key == OIS::KC_G)   // toggle visibility of even rarer debugging details
    {
        if (mDetailsPanel->getTrayLocation() == OgreBites::TL_NONE)
        {
            mTrayMgr->moveWidgetToTray(mDetailsPanel, OgreBites::TL_TOPRIGHT, 0);
            mDetailsPanel->show();
        }
        else
        {
            mTrayMgr->removeWidgetFromTray(mDetailsPanel);
            mDetailsPanel->hide();
        }
    }
    else if (arg.key == OIS::KC_T)   // cycle polygon rendering mode
    {
        Ogre::String newVal;
        Ogre::TextureFilterOptions tfo;
        unsigned int aniso;

        switch (mDetailsPanel->getParamValue(9).asUTF8()[0])
        {
        case 'B':
            newVal = "Trilinear";
            tfo = Ogre::TFO_TRILINEAR;
            aniso = 1;
            break;
        case 'T':
            newVal = "Anisotropic";
            tfo = Ogre::TFO_ANISOTROPIC;
            aniso = 8;
            break;
        case 'A':
            newVal = "None";
            tfo = Ogre::TFO_NONE;
            aniso = 1;
            break;
        default:
            newVal = "Bilinear";
            tfo = Ogre::TFO_BILINEAR;
            aniso = 1;
        }

        Ogre::MaterialManager::getSingleton().setDefaultTextureFiltering(tfo);
        Ogre::MaterialManager::getSingleton().setDefaultAnisotropy(aniso);
        mDetailsPanel->setParamValue(9, newVal);
    }
    else if (arg.key == OIS::KC_R)   // cycle polygon rendering mode
    {
        Ogre::String newVal;
        Ogre::PolygonMode pm;

        switch (mCamera->getPolygonMode())
        {
        case Ogre::PM_SOLID:
            newVal = "Wireframe";
            pm = Ogre::PM_WIREFRAME;
            break;
        case Ogre::PM_WIREFRAME:
            newVal = "Points";
            pm = Ogre::PM_POINTS;
            break;
        default:
            newVal = "Solid";
            pm = Ogre::PM_SOLID;
        }

        mCamera->setPolygonMode(pm);
        mDetailsPanel->setParamValue(10, newVal);
    }
    else if(arg.key == OIS::KC_F5)   // refresh all textures
    {
        Ogre::TextureManager::getSingleton().reloadAll();
    }
    else if (arg.key == OIS::KC_SYSRQ)   // take a screenshot
    {
        mWindow->writeContentsToTimestampedFile("screenshot", ".jpg");
    }
    else if (arg.key == OIS::KC_ESCAPE)
    {
        mShutDown = true;
    }

    mCameraMan->injectKeyDown(arg);
    return true;
}
//---------------------------------------------------------------------------
bool BaseApplication::keyReleased(const OIS::KeyEvent &arg)
{
    mCameraMan->injectKeyUp(arg);
    return true;
}
//---------------------------------------------------------------------------
bool BaseApplication::mouseMoved(const OIS::MouseEvent &arg)
{
#if OGRE_VERSION_MINOR == 10
    if (mTrayMgr->injectPointerMove(arg)) return true;
    mCameraMan->injectPointerMove(arg);
#else
    if (mTrayMgr->injectMouseMove(arg)) return true;
    mCameraMan->injectMouseMove(arg);
#endif
    return true;
}
//---------------------------------------------------------------------------
bool BaseApplication::mousePressed(const OIS::MouseEvent &arg, OIS::MouseButtonID id)
{
#if OGRE_VERSION_MINOR == 10
    if (mTrayMgr->injectPointerDown(arg, id)) return true;
    mCameraMan->injectPointerDown(arg, id);
#else
    if (mTrayMgr->injectMouseDown(arg, id)) return true;
    mCameraMan->injectMouseDown(arg, id);
#endif
    return true;
}
//---------------------------------------------------------------------------
bool BaseApplication::mouseReleased(const OIS::MouseEvent &arg, OIS::MouseButtonID id)
{
#if OGRE_VERSION_MINOR == 10
    if (mTrayMgr->injectPointerUp(arg, id)) return true;
    mCameraMan->injectPointerUp(arg, id);
#else
    if (mTrayMgr->injectMouseUp(arg, id)) return true;
    mCameraMan->injectMouseUp(arg, id);
#endif
    return true;
}
//---------------------------------------------------------------------------
// Adjust mouse clipping area
void BaseApplication::windowResized(Ogre::RenderWindow* rw)
{
    unsigned int width, height, depth;
    int left, top;
    rw->getMetrics(width, height, depth, left, top);

    const OIS::MouseState &ms = mMouse->getMouseState();
    ms.width = width;
    ms.height = height;
}
//---------------------------------------------------------------------------
// Unattach OIS before window shutdown (very important under Linux)
void BaseApplication::windowClosed(Ogre::RenderWindow* rw)
{
    // Only close for window that created OIS (the main window in these demos)
    if(rw == mWindow)
    {
        if(mInputManager)
        {
            mInputManager->destroyInputObject(mMouse);
            mInputManager->destroyInputObject(mKeyboard);

            OIS::InputManager::destroyInputSystem(mInputManager);
            mInputManager = 0;
        }
    }
}
//---------------------------------------------------------------------------