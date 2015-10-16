/**
 * D adaptation of the Qt5 tutorial at: http://doc.qt.io/qt-4.8/qt-mainwindows-application-example.html
 *
 * Module map files from utils/modulemap/ should be installed in the libc and Qt include folders.
 *
 * Modify the build lines below for your version of Qt where needed. Or the Makefile.
 *
 * Then build with make or the following equivalent:
 *   $ make

 *   $ clang++ -O3 -fPIC -c moc_cpp-qt5demo.cpp -I/usr/include/qt5/

 *   $ clang++ -O3 -fPIC -c cpp-qt5demo.cpp -I/usr/include/qt5/

 *   $ ldc2 -wi -v moc_cpp-qt5demo.o cpp-qt5demo.o -cpp-args -D_REENTRANT -cpp-args -fPIE -cpp-args -DQT_WIDGETS_LIB -cpp-args -DQT_GUI_LIB -cpp-args -DQT_CORE_LIB -cpp-args -I/path/to/Qt/5.4/gcc_64/mkspecs/linux-g++ -cpp-args -I/path/to/Qt/5.4/gcc_64/include -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtWidgets -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtGui -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtCore -L-rpath=/path/to/Qt/5.4/gcc_64 -L-rpath=/path/to/Qt/5.4/gcc_64/lib -L-lQt5Widgets -L-lQt5Gui -L-lQt5Core -L-lGL -L-lstdc++ qt5demo.d
 */

modmap (C++) "<qglobal.h>";
modmap (C++) "<QtWidgets>";
modmap (C++) "cpp-qt5demo.hpp";

// D imports
import core.runtime;
import std.stdio, std.conv;
import std.signals;

import (C++) Qt.QtCore;
import (C++) Qt.AlignmentFlag;

// Main Qt imports
import (C++) QApplication, QString, QPushButton, QAction, QMainWindow;
import (C++) QWidget, QTextEdit, QLineEdit, QLabel, QLayout, QGridLayout;
import (C++) QCloseEvent, QPlainTextEdit, QMenu, QToolBar, QMessageBox, QFlags, QFileDialog;
import (C++) QIcon, QSettings, QPoint, QSize, QVariant, QFile, QIODevice, QMenuBar, QStatusBar;
import (C++) QFileInfo, QMetaMethod, QObject, QByteArray, QKeySequence, QTextStream;

// demo namespace imports
import (C++) demo._;
import (C++) demo.DLangMainWindow;
import (C++) demo.CPPactions;

// Enums
import (C++) Qt.ConnectionType;


class MainWindow : DLangMainWindow
{
public:
    this(QWidget *parent = null)
    {
        parentWin = parent;

	// Initialize textEdit and the actions before calling the super constructor
        textEdit = new QPlainTextEdit();
        createActions();

	// For some reason calling the super constructor below with more than
	// 7 parameters doesn't work. Use this struct instead. We could just use
	// the struct but I will leave this as an example of two ways to do this.
	CPPactions cppactions;
	cppactions.saveAct = saveAct;
	cppactions.saveAsAct = saveAsAct;
	cppactions.exitAct = exitAct;
	cppactions.aboutAct = aboutAct;
	cppactions.aboutQtAct = aboutQtAct;

	super(textEdit, copyAct, cutAct, pasteAct, newAct, openAct, &cppactions);

	// Attach D signals to the corresponding D functions
	this.textContentsChanged.connect(&this.documentWasModified);
	this.newFileAction.connect(&this.newFile);
	this.openFileAction.connect(&this.open);
	this.saveFileAction.connect(&this.save);
	this.saveAsFileAction.connect(&this.saveAs);
	this.aboutFileAction.connect(&this.about);

	// Pretty much the same as the Qt example from here down for the constructor
        setCentralWidget(cast(QWidget *) textEdit);

        createMenus();
        createToolBars();
        createStatusBar();
        readSettings();

        setCurrentFile(QString(""));
        setUnifiedTitleAndToolBarOnMac(true);
    }

    // Name our D signals so we can attach an observer (slot) to each one
    // individually. We need a single paramter for the second through sixth
    // emit functions or we get an error when building?? Not sure why
    mixin Signal!()     textContentsChanged;
    mixin Signal!(bool) newFileAction;
    mixin Signal!(bool) openFileAction;
    mixin Signal!(bool) saveFileAction;
    mixin Signal!(bool) saveAsFileAction;
    mixin Signal!(bool) aboutFileAction;

    // Sort of dummy emit functions to just pass through the Qt signals
    override void DLangContentsChanged() {
	textContentsChanged.emit();
    }
    override void DLangNewFile() {
	newFileAction.emit(false);
    }
    override void DLangOpenFile() {
	openFileAction.emit(false);
    }
    override void DLangSaveFile() {
	saveFileAction.emit(false);
    }
    override void DLangSaveAsFile() {
	saveAsFileAction.emit(false);
    }
    override void DLangAboutFile() {
	aboutFileAction.emit(false);
    }


    override void closeEvent(QCloseEvent *event)
    {
        if (maybeSave())
        {
	    writeSettings();
	    event.accept();
	}
        else
	    event.ignore();
    }

    void newFile(bool b)
    {
        if (maybeSave())
	{
	    textEdit.clear();
	    setCurrentFile(QString(""));
	}
    }

    void open(bool a)
    {
	if(maybeSave())
	{
	    QString fileName = QFileDialog.getOpenFileName(cast(QWidget *)parentWin);
	    if (!fileName.isEmpty())
	        loadFile(fileName);
	}
    }

    void save(bool a)
    {
        if (curFile.isEmpty())
	{
	    saveAs(false);
	    return;
	}
	else
	{
	    saved = saveFile(*curFile);
	    return;
	}
    }

    void saveAs(bool a)
    {
        QString fileName = QFileDialog.getSaveFileName(cast(QWidget *)parentWin);
	if (fileName.isEmpty())
	{
	    saved = false;
	    return;
	}
	saved = saveFile(fileName);
	return;
    }

    void about(bool a)
    {
	auto AAString = QString("About Application");
	auto APPString = QString("This <b>Application</b> " ~ 
	     "example demonstrates how to write modern GUI applications in D " ~
	     "using QT and Calypso, with a menu bar, toolbars, and a status bar.");
        QMessageBox.about(cast(QWidget*)parentWin, AAString, APPString);
    }

    void documentWasModified()
    {
        auto ted = textEdit.document();
        setWindowModified(ted.isModified());
    }

    void createActions()
    {
	alias StandardKey = QKeySequence.StandardKey;

	// File Menu actions. 

	// We need to use the next four lines to initialize our QStrings and QIcon
	// otherwise we would run into this problem with D:
	// http://forum.dlang.org/thread/mm81tt$bph$1@digitalmars.com
	// UGLY but it works
	auto NewIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/new.png");
	auto NewNString = QString("&New");
	auto NewIcon = QIcon(NewIString);
	auto NewCString = QString("Create a new File");
        newAct = new QAction(NewIcon, NewNString, cast(QObject*)parentWin);
	newAct.setShortcuts(StandardKey.New);
	newAct.setStatusTip(NewCString);

	auto OpenIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/open.png");
	auto OpenNString = QString("&Open");
	auto OpenIcon = QIcon(OpenIString);
        openAct = new QAction(OpenIcon, OpenNString, cast(QObject*)parentWin);
	openAct.setShortcuts(StandardKey.Open);
	auto OpenCString = QString("Open an existing file");
	openAct.setStatusTip(OpenCString);

	auto SaveIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/save.png");
	auto SaveNString = QString("&Save");
	auto SaveIcon = QIcon(SaveIString);
        saveAct = new QAction(SaveIcon, SaveNString, cast(QObject*)parentWin);
	saveAct.setShortcuts(StandardKey.Save);
	auto SaveCString = QString("Save file");
	saveAct.setStatusTip(SaveCString);

	auto SaveAsNString = QString("Save &As...");
        saveAsAct = new QAction(SaveAsNString, cast(QObject*)parentWin);
	saveAsAct.setShortcuts(StandardKey.SaveAs);
	auto SaveAsCString = QString("Save file under new name");
	saveAsAct.setStatusTip(SaveAsCString);

	auto ExitNString = QString("E&xit");
        exitAct = new QAction(ExitNString, cast(QObject*)parentWin);
	exitAct.setShortcuts(StandardKey.Close);
	auto ExitCString = QString("Exit application");
	exitAct.setStatusTip(ExitCString);

	// Edit Menu actions
	auto CopyIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/copy.png");
	auto CopyNString = QString("&Copy");
	auto CopyIcon = QIcon(CopyIString);
        copyAct = new QAction(CopyIcon, CopyNString, cast(QObject*)parentWin);
	copyAct.setShortcuts(StandardKey.Copy);
	auto CopyCString = QString("Copy text and save it to clipboard");
	copyAct.setStatusTip(CopyCString);

	auto CutIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/cut.png");
	auto CutNString = QString("&Cut");
	auto CutIcon = QIcon(CutIString);
        cutAct = new QAction(CutIcon, CutNString, cast(QObject*)parentWin);
	cutAct.setShortcuts(StandardKey.Cut);
	auto CutCString = QString("Cut text and save it to clipboard");
	cutAct.setStatusTip(CutCString);

	auto PasteIString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/paste.png");
	auto PasteNString = QString("&Paste");
	auto PasteIcon = QIcon(PasteIString);
        pasteAct = new QAction(PasteIcon, PasteNString, cast(QObject*)parentWin);
	pasteAct.setShortcuts(StandardKey.Paste);
	auto PasteCString = QString("Paste text from clipboard");
	pasteAct.setStatusTip(PasteCString);

	// Help Menu actions
	auto AboutNString = QString("&About");
        aboutAct = new QAction(AboutNString, cast(QObject*)parentWin);
	auto AboutCString = QString("About the Application");
	aboutAct.setStatusTip(AboutCString);

	auto AboutQtNString = QString("About &Qt");
        aboutQtAct = new QAction(AboutQtNString, cast(QObject*)parentWin);
	auto AboutQtCString = QString("Show the Qt Library's About Box");
	aboutQtAct.setStatusTip(AboutQtCString);

	copyAct.setEnabled(false);
	cutAct.setEnabled(false);
    }

    void createMenus()
    {
	auto FString = QString("&File");
        fileMenu = (cast(QMenuBar*)menuBar()).addMenu(FString);

	alias QWP = QWidget*;

	// ERROR: with statement doesn't work. LLVM code gen error
	/*
	  with (cast(QWP)fMenu) {
	    addAction(newAct);
            addAction(openAct);
            addAction(saveAct);
            addAction(saveAsAct);
	  }
	*/
	(cast(QWP)fileMenu).addAction(newAct);
	(cast(QWP)fileMenu).addAction(openAct);
	(cast(QWP)fileMenu).addAction(saveAct);
	(cast(QWP)fileMenu).addAction(saveAsAct);
	fileMenu.addSeparator();
	(cast(QWP)fileMenu).addAction(exitAct);

	auto EString = QString("&Edit");
        editMenu = (cast(QMenuBar*)menuBar()).addMenu(EString);
	(cast(QWP)editMenu).addAction(cutAct);
	(cast(QWP)editMenu).addAction(copyAct);
	(cast(QWP)editMenu).addAction(pasteAct);

	menuBar.addSeparator();

	auto AString = QString("&Help");
        helpMenu = (cast(QMenuBar*)menuBar()).addMenu(AString);
	(cast(QWP)helpMenu).addAction(aboutAct);
	(cast(QWP)helpMenu).addAction(aboutQtAct);
    }

    void createToolBars()
    {
	auto FString = QString("File");
	fileToolBar = addToolBar(FString);

	alias QWP = QWidget*;

	// OPTION:  We could set up an action with the following four lines manually, if needed
	// auto IString = QString("/home/wilsonk/Downloads/Calypso/tests/calypso/qt5/images/new.png");
	// auto NString = QString("&New");
	// auto Icon = QIcon(IString);
	// The last parameter is a text string representing the slot to call, so it must
	// have the '1' in front of it to trick Qt into thinking it is a slot.
	// fileToolBar.addAction(Icon, NString, cast(QObject*)this, cast(char*)"1DLangNewFile(bool)");

	(cast(QWP)fileToolBar).addAction(newAct);
	(cast(QWP)fileToolBar).addAction(openAct);
	(cast(QWP)fileToolBar).addAction(saveAct);

	auto EString = QString("Edit");
	editToolBar = addToolBar(EString);

	(cast(QWP)editToolBar).addAction(cutAct);
	(cast(QWP)editToolBar).addAction(copyAct);
	(cast(QWP)editToolBar).addAction(pasteAct);
    }

    void createStatusBar()
    {
	auto RString = QString("Ready");
     	super.statusBar().showMessage(RString);
    }

    void readSettings()
    {
	auto TString = QString("Trolltech");
	auto AEString = QString("Application Example");
	QSettings settings = QSettings(TString, AEString);

        // This ugliness is needed because of issue http://forum.dlang.org/thread/mm81tt$bph$1@digitalmars.com
	// See shorter version in original example
	auto PosXString = QString("posX");
	auto PosYString = QString("posY");
	auto SizeXString = QString("sizeX");
	auto SizeYString = QString("sizeY");
	auto twoHundred = QVariant(200);
	auto sixHundred = QVariant(600);

	// ERROR: these settings aren't actually being set so the default values are needed here
	auto posX = settings.value(PosXString, twoHundred).toInt();
	auto posY = settings.value(PosYString, twoHundred).toInt();
	auto sizeX = settings.value(SizeXString, sixHundred).toInt();
	auto sizeY = settings.value(SizeYString, sixHundred).toInt();

	auto point = QPoint(posX,posY);
	auto qsize = QSize(sizeX,sizeY);
	resize(qsize);
	move(point);
    }

    void writeSettings()
    {
	auto TString = QString("Trolltech");
	auto AEString = QString("Application Example");
	QSettings settings = QSettings(TString, AEString);

        // This ugliness is needed because of issue http://forum.dlang.org/thread/mm81tt$bph$1@digitalmars.com
	// See shorter version in original example
	// ERROR: the settings aren't being set?! No idea why...I thought there was a default
	// output file...maybe we have to designate and open a file?
	auto PosXString = QString("posX");
	auto posX = QVariant(pos().x);
	settings.setValue(PosXString, posX);

	auto PosYString = QString("posY");
	auto posY = QVariant(pos().y);
	settings.setValue(PosXString, posY);

	auto SizeXString = QString("sizeX");
	auto sizeX = QVariant(size().width);
	settings.setValue(SizeXString, sizeX);

	auto SizeYString = QString("sizeY");
	auto sizeY = QVariant(size().height);
	settings.setValue(SizeYString, sizeY);

    }

    bool maybeSave()
    {
        if (textEdit.document().isModified())
	{
	    alias StandardButton = QMessageBox.StandardButton;
	    StandardButton ret;

            auto flags = QFlags!(QMessageBox.StandardButton)(StandardButton.Save | StandardButton.Discard | StandardButton.Cancel);

	    auto AString = QString("Application");
	    auto MODString = QString("The document has been modified, Do you want to save?");
	    ret = cast(StandardButton) (QMessageBox.warning(parentWin, AString, MODString, flags));

            if (ret == StandardButton.Save)
	    {
	        save(false);
		return saved;
	    }
	    else if (ret == StandardButton.Cancel)
	        return false;
	}
        return true;
    }

    void loadFile(const	QString fileName)
    {
	auto file = QFile(fileName);
	QByteArray ba = fileName.toLatin1();

        // ERROR: Can't seem to just do 'if (!file.open(omf.ReadOnly | omf.Text...'.
        // Maybe import enum directly first?? Leaving this way as example workaround
	alias omf = QIODevice.OpenModeFlag;
	auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.ReadOnly | omf.Text);

	if (!file.open(flags))
	{
            // ERROR: the following segfaults so reworked in D below
            //auto x = new QString("Cannot read file %1: \n%2").arg(fileName).arg(file.errorString());
	    auto dString = to!string("Cannot read file ") ~ 
		to!string(ba.data()) ~ " " ~ 
		to!string(file.errorString().toLatin1.data());

	    auto AString = QString("Application");
	    auto charPString = QString(cast(char*)dString);
	    QMessageBox.warning(parentWin, AString, charPString);

	    return;
	}

	auto inFile = new QTextStream(cast(QIODevice*)&file);
	auto inf = inFile.readAll();
	textEdit.setPlainText(inf);

	setCurrentFile(fileName);
	auto FLString = QString("File Loaded");
	super.statusBar().showMessage(FLString, 2000);
    }

    bool saveFile(const	QString fileName)
    {
	// Write file out in D instead of Qt. Error check in Qt, though.
	QString str = textEdit.toPlainText();
	QByteArray ba = str.toLatin1();
	auto text = to!string(ba.data());

	auto file = QFile(fileName);
	ba = fileName.toLatin1();

        alias omf = QIODevice.OpenModeFlag;
        auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.WriteOnly | omf.Text);

        if (!file.open(flags)) {

	    // ERROR: the following segfaults so reworked in D below
	    // auto x = new QString("Cannot read file %1: \n%2").arg(fileName).arg(file.errorString());
	    auto dString = to!string("Cannot write file ") ~ 
		to!string(ba.data()) ~ " " ~ 
		to!string(file.errorString().toLatin1.data());

	    auto AString = QString("Application");
	    auto charPString = QString(cast(char*)dString);
	    QMessageBox.warning(parentWin, AString, charPString);

	    return false;
	}
	
	auto f = File(to!string(ba.data()), "w");
	f.writeln(cast(char[])text);
	f.close();
	file.close();

	setCurrentFile(fileName);
	auto FLString = QString("File saved");
	super.statusBar().showMessage(FLString, 2000);
        return true;
    }

    void setCurrentFile(const QString fileName)
    {
        curFile = new QString(fileName);
	auto ted = textEdit.document();

	ted.setModified(false);
	setWindowModified(false);

	QString showName = QString(*curFile);
	if (curFile.isEmpty())
	    showName = QString("untitled.txt");

        setWindowFilePath(showName);
    }

    QString strippedName(const QString fullFileName)
    {
	auto qfi = new QFileInfo(fullFileName);
	return qfi.fileName();
    }

private:
    bool                saved;
    QPlainTextEdit      *textEdit;
    QWidget 	   	*parentWin;
    QString		*curFile;

    QMenu		*fileMenu, editMenu, helpMenu;
    QToolBar 		*fileToolBar, editToolBar;
    QAction 		*newAct, openAct, saveAct, saveAsAct, exitAct, copyAct, cutAct, pasteAct, aboutAct, aboutQtAct;
}

int main()
{
    auto app = new QApplication(Runtime.cArgs.argc, Runtime.cArgs.argv);

    auto CString = QString("Calypso");
    auto AEString = QString("Example App");
    app.setOrganizationName(CString);
    app.setApplicationName(AEString);

    auto mainWin = new MainWindow;
    mainWin.show();

    return app.exec();
}
