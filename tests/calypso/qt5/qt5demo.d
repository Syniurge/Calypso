/**
 * D adaptation of the Qt5 tutorial at: http://doc.qt.io/qt-5/qtwidgets-mainwindows-application-example.html
 *
 * Module map files from utils/modulemap/ should be installed in the libc and Qt include folders.
 *
 * Then build with:
 *   $ ldc2 -relocation-model=pic -wi -v -cpp-args -D_REENTRANT -cpp-args -fPIE -cpp-args -DQT_WIDGETS_LIB -cpp-args -DQT_GUI_LIB -cpp-args -DQT_CORE_LIB -cpp-args -I/pathto/Qt/5.5/gcc_64/mkspecs/linux-g++ -cpp-args -I/pathto/Qt/5.5/gcc_64/include -cpp-args -I/pathto/Qt/5.5/gcc_64/include/QtWidgets -cpp-args -I/pathto/Qt/5.5/gcc_64/include/QtGui -cpp-args -I/pathto/Qt/5.5/gcc_64/include/QtCore -L-rpath=/pathto/Qt/5.5/gcc_64 -L-rpath=/pathto/Qt/5.5/gcc_64/lib -L-lQt5Widgets -L-lQt5Gui -L-lQt5Core -L-lGL -L-lstdc++ qt5demo.d moc/package.d moc/moc_.d moc/types.d
 */

 // WORKAROUND until modmap gets fixed
modmap (C++) "<QtCore>";
modmap (C++) "<private/qmetaobject_p.h>";
modmap (C++) "<qglobal.h>";
modmap (C++) "<qmetatype.h>";

modmap (C++) "<QtWidgets>";

// D imports
import moc;
import core.runtime;
import std.stdio, std.conv;

// Main Qt imports
import (C++) Qt.QtCore;
import (C++) QCoreApplication, QApplication, QString, QPushButton, QAction, QMainWindow;
import (C++) QWidget, QTextEdit, QLineEdit, QLabel, QLayout, QGridLayout, QTextDocument;
import (C++) QCloseEvent, QPlainTextEdit, QMenu, QToolBar, QMessageBox, QFlags;
import (C++) QIcon, QSettings, QPoint, QSize, QVariant, QFile, QIODevice, QMenuBar;
import (C++) QFileInfo, QMetaMethod, QObject, QByteArray, QKeySequence;
import (C++) QCursor, QFileDialog, QStatusBar, QTextStream;

// Enums
import (C++) Qt.AlignmentFlag, Qt.CursorShape, Qt.WindowModality;

enum HAS_QT_NO_CURSOR = __traits(compiles, QT_NO_CURSOR);

class MainWindow : QMainWindow
{
public:
    // Two test signals to check if the metaobject was generated properly
    // NOTE: Qt signals may have a return type different from void
    mixin signal!("testSignalNew", void function());
    mixin signal!("testSignalOpen", void function());

    mixin Q_OBJECT;

    this(QWidget *parent = null)
    {
        super(parent);

        textEdit = new QPlainTextEdit;
        setCentralWidget(textEdit);

        createActions();
        createMenus();
        createToolBars();
        createStatusBar();

        readSettings();

        connect2!(QTextDocument.contentsChanged, documentWasModified)(textEdit.document(), this);

        setCurrentFile(QString(""));
        setUnifiedTitleAndToolBarOnMac(true);
    }

    extern(C++) override void closeEvent(QCloseEvent *event)
    {
        if (maybeSave())
        {
            writeSettings();
            event.accept();
        }
        else
            event.ignore();
    }

public extern(C++)  @slots
{
    void newFile()
    {
        if (maybeSave())
        {
            textEdit.clear();
            setCurrentFile(QString(""));
        }

        testSignalNew(); // emit the 'new' test signal
    }

    void open()
    {
        if(maybeSave())
        {
            auto fileName = QFileDialog.getOpenFileName(this);
            if (!fileName.isEmpty())
                loadFile(fileName);
        }

        testSignalOpen(); // emit the 'open' test signal
    }

    bool save()
    {
        if (!curFile || curFile.isEmpty())
            return saveAs();
        else
            return saveFile(*curFile);
    }

    bool saveAs()
    {
        auto dialog = QFileDialog(this);
        dialog.setWindowModality(WindowModality.WindowModal);
        dialog.setAcceptMode(QFileDialog.AcceptMode.AcceptSave);
        if (dialog.exec())
            return saveFile(dialog.selectedFiles().at(0));
        else
            return false;
    }

    void about()
    {
        auto AAString = QString("About Application");
        auto APPString = QString("This <b>Application</b> " ~
            "example demonstrates how to write modern GUI applications in D " ~
            "using QT and Calypso, with a menu bar, toolbars, and a status bar.");
        QMessageBox.about(this, AAString, APPString);
    }

    void documentWasModified()
    {
        setWindowModified(textEdit.document().isModified());
    }
} // end of extern(C++)  @slots

    void createActions()
    {
        alias StandardKey = QKeySequence.StandardKey;

        // UGLY: we need to use the next four lines to initialize our QStrings and QIcon
        // otherwise we would run into this problem with D:
        // http://forum.dlang.org/thread/mm81tt$bph$1@digitalmars.com
        auto NewIString = QString("images/new.png"); auto NewNString = QString("&New");
        auto NewIcon = QIcon(NewIString); auto NewCString = QString("Create a new File");
            newAct = new QAction(NewIcon, NewNString, this);
        newAct.setShortcuts(StandardKey.New);
        newAct.setStatusTip(NewCString);
        connect2!(QAction.triggered, newFile)(newAct, this);

        auto OpenIString = QString("images/open.png"); auto OpenNString = QString("&Open");
        auto OpenIcon = QIcon(OpenIString); auto OpenCString = QString("Open an existing file");
            openAct = new QAction(OpenIcon, OpenNString, this);
        openAct.setShortcuts(StandardKey.Open);
        openAct.setStatusTip(OpenCString);
        connect2!(QAction.triggered, open)(openAct, this);

        auto SaveIString = QString("images/save.png"); auto SaveNString = QString("&Save");
        auto SaveIcon = QIcon(SaveIString); auto SaveCString = QString("Save file");
            saveAct = new QAction(SaveIcon, SaveNString, this);
        saveAct.setShortcuts(StandardKey.Save);
        saveAct.setStatusTip(SaveCString);
        connect2!(QAction.triggered, save)(saveAct, this);

        auto SaveAsNString = QString("Save &As..."); auto SaveAsCString = QString("Save file under new name");
            saveAsAct = new QAction(SaveAsNString, this);
        saveAsAct.setShortcuts(StandardKey.SaveAs);
        saveAsAct.setStatusTip(SaveAsCString);
        connect2!(QAction.triggered, saveAs)(saveAsAct, this);

        auto ExitNString = QString("E&xit"); auto ExitCString = QString("Exit application");
            exitAct = new QAction(ExitNString, this);
        exitAct.setShortcuts(StandardKey.Close);
        exitAct.setStatusTip(ExitCString);
        connect2!(QAction.triggered, close)(exitAct, this);

        // Edit Menu actions
        auto CopyIString = QString("images/copy.png"); auto CopyNString = QString("&Copy");
        auto CopyIcon = QIcon(CopyIString); auto CopyCString = QString("Copy text and save it to clipboard");
            copyAct = new QAction(CopyIcon, CopyNString, this);
        copyAct.setShortcuts(StandardKey.Copy);
        copyAct.setStatusTip(CopyCString);
        connect2!(QAction.triggered, QPlainTextEdit.copy)(copyAct, textEdit);

        auto CutIString = QString("images/cut.png"); auto CutNString = QString("&Cut");
        auto CutIcon = QIcon(CutIString); auto CutCString = QString("Cut text and save it to clipboard");
            cutAct = new QAction(CutIcon, CutNString, this);
        cutAct.setShortcuts(StandardKey.Cut);
        cutAct.setStatusTip(CutCString);
        connect2!(QAction.triggered, QPlainTextEdit.cut)(cutAct, textEdit);

        auto PasteIString = QString("images/paste.png"); auto PasteNString = QString("&Paste");
        auto PasteIcon = QIcon(PasteIString); auto PasteCString = QString("Paste text from clipboard");
            pasteAct = new QAction(PasteIcon, PasteNString, this);
        pasteAct.setShortcuts(StandardKey.Paste);
        pasteAct.setStatusTip(PasteCString);
        connect2!(QAction.triggered, QPlainTextEdit.paste)(pasteAct, textEdit);

        // Help Menu actions
        auto AboutNString = QString("&About"); auto AboutCString = QString("About the Application");
            aboutAct = new QAction(AboutNString, this);
        aboutAct.setStatusTip(AboutCString);
        connect2!(QAction.triggered, about)(aboutAct, this);

        auto AboutQtNString = QString("About &Qt"); auto AboutQtCString = QString("Show the Qt Library's About Box");
            aboutQtAct = new QAction(AboutQtNString, this);
        aboutQtAct.setStatusTip(AboutQtCString);
        connect2!(QAction.triggered, QApplication.aboutQt)(aboutQtAct, QCoreApplication.instance());

        copyAct.setEnabled(false);
        cutAct.setEnabled(false);
        connect2!(QPlainTextEdit.copyAvailable, QAction.setEnabled)(textEdit, cutAct);
        connect2!(QPlainTextEdit.copyAvailable, QAction.setEnabled)(textEdit, copyAct);
    }

    void createMenus()
    {
        auto FString = QString("&File");
            fileMenu = menuBar().addMenu(FString);

        fileMenu.QWidget.addAction(newAct);
        fileMenu.QWidget.addAction(openAct);
        fileMenu.QWidget.addAction(saveAct);
        fileMenu.QWidget.addAction(saveAsAct);
        fileMenu.addSeparator();
        fileMenu.QWidget.addAction(exitAct);

        auto EString = QString("&Edit");
            editMenu = menuBar().addMenu(EString);
        editMenu.QWidget.addAction(cutAct);
        editMenu.QWidget.addAction(copyAct);
        editMenu.QWidget.addAction(pasteAct);

        menuBar.addSeparator();

        auto AString = QString("&Help");
            helpMenu = menuBar().addMenu(AString);
        helpMenu.QWidget.addAction(aboutAct);
        helpMenu.QWidget.addAction(aboutQtAct);
    }

    void createToolBars()
    {
        auto FString = QString("File");
        fileToolBar = addToolBar(FString);

        fileToolBar.QWidget.addAction(newAct);
        fileToolBar.QWidget.addAction(openAct);
        fileToolBar.QWidget.addAction(saveAct);

        auto EString = QString("Edit");
        editToolBar = addToolBar(EString);

        editToolBar.QWidget.addAction(cutAct);
        editToolBar.QWidget.addAction(copyAct);
        editToolBar.QWidget.addAction(pasteAct);
    }

    void createStatusBar()
    {
        auto RString = QString("Ready");
        statusBar().showMessage(RString);
    }

    void readSettings()
    {
        auto TString = QString("Trolltech"); auto AEString = QString("Application Example");
        QSettings settings = QSettings(TString, AEString);

        auto PosString = QString("pos"); auto PosPoint = QPoint(200, 200); auto PosPointV = QVariant(PosPoint);
        auto SizeString = QString("size"); auto SizeSize = QSize(400, 400); auto SizeSizeV = QVariant(SizeSize);

        QPoint pos = settings.value(PosString, PosPointV).toPoint();
        QSize size = settings.value(SizeString, SizeSizeV).toSize();
        resize(size);
        move(pos);
    }

    void writeSettings()
    {
        auto TString = QString("Trolltech"); auto AEString = QString("Application Example");
        QSettings settings = QSettings(TString, AEString);

        auto PosString = QString("pos"); auto Pos = pos(); auto PosV = QVariant(Pos);
        settings.setValue(PosString, PosV);

        auto SizeString = QString("size"); auto Size = size(); auto SizeV = QVariant(Size);
        settings.setValue(SizeString, SizeV);
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
            ret = cast(StandardButton) QMessageBox.warning(this, AString, MODString, flags);

            if (ret == StandardButton.Save)
                return save();
            else if (ret == StandardButton.Cancel)
                return false;
        }
        return true;
    }

    void loadFile(const QString fileName)
    {
        auto file = QFile(fileName);

        alias omf = QIODevice.OpenModeFlag;
        auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.ReadOnly | omf.Text);

        if (!file.open(flags))
        {
            auto AString = QString("Application");
            auto FileErrorString = file.errorString();
//             auto WarningString = QString("Cannot read file %1: \n%2").arg(fileName).arg(FileErrorString);
            // DMD/CALYPSO BUG: QString.arg() cannot be used for now without triggering a forward referencing error.
            // DMD's Struct/ClassDeclaration.semantic() need to be more solid for complex libraries (both C++ and D,
            // see https://issues.dlang.org/show_bug.cgi?id=7426 which has never really been fixed)
            auto d_WarningString = "Cannot read file " ~
                    to!string(fileName.toLatin1.data()) ~ ": \n" ~
                    to!string(file.errorString().toLatin1.data());
            auto WarningString = QString(d_WarningString.ptr);

            QMessageBox.warning(this, AString, WarningString);
            return;
        }

        auto inFile = QTextStream(&file);

        static if (!HAS_QT_NO_CURSOR)
        {
            auto cursor = QCursor(CursorShape.WaitCursor);
            QApplication.setOverrideCursor(cursor);
        }

        auto inf = inFile.readAll();
        textEdit.setPlainText(inf);

        static if (!HAS_QT_NO_CURSOR)
            QApplication.restoreOverrideCursor();

        setCurrentFile(fileName);
        auto FLString = QString("File Loaded");
        statusBar().showMessage(FLString, 2000);
    }

    bool saveFile(const QString fileName)
    {
        // Write file out in D instead of Qt. Error check in Qt, though.
        QString str = textEdit.toPlainText();
        auto text = to!string(str.toLatin1().data());

        auto file = QFile(fileName);
        QByteArray ba = fileName.toLatin1();

        alias omf = QIODevice.OpenModeFlag;
        auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.WriteOnly | omf.Text);

        if (!file.open(flags)) {
            auto AString = QString("Application");
            auto FileErrorString = file.errorString();
//             auto WarningString = QString("Cannot write file %1: \n%2").arg(fileName).arg(FileErrorString);
            auto d_WarningString = "Cannot write file " ~
                    to!string(fileName.toLatin1.data()) ~ ": \n" ~
                    to!string(file.errorString().toLatin1.data());
            auto WarningString = QString(d_WarningString.ptr);

            QMessageBox.warning(this, AString, WarningString);
            return false;
        }

        static if (!HAS_QT_NO_CURSOR)
        {
            auto cursor = QCursor(CursorShape.WaitCursor);
            QApplication.setOverrideCursor(cursor);
        }

        auto f = File(to!string(ba.data()), "w");
        f.writeln(cast(char[])text);
        f.close();
        file.close();

        static if (!HAS_QT_NO_CURSOR)
            QApplication.restoreOverrideCursor();

        setCurrentFile(fileName);
        auto FLString = QString("File saved");
        statusBar().showMessage(FLString, 2000);
        return true;
    }

    void setCurrentFile(const QString fileName)
    {
        if (!curFile)
            curFile = new QString;

        *curFile = fileName;
        textEdit.document().setModified(false);
        setWindowModified(false);

        auto showName = QString(*curFile);
        if (curFile.isEmpty())
            showName = "untitled.txt";

        setWindowFilePath(showName);
    }

    QString strippedName(const QString fullFileName)
    {
        return QFileInfo(fullFileName).fileName();
    }

private:
    QPlainTextEdit* textEdit;
    QString* curFile;

    QMenu* fileMenu, editMenu, helpMenu;
    QToolBar* fileToolBar, editToolBar;
    QAction* newAct, openAct, saveAct, saveAsAct, exitAct, copyAct, cutAct, pasteAct, aboutAct, aboutQtAct;
}

// Test class receiving signals emitted by MainWindows
class TestReceiver : QObject
{
    mixin Q_OBJECT;

public extern(C++) @slots:
    void recvNew() { writeln("Received 'new' test signal"); }
    void recvOpen() { writeln("Received 'open' test signal"); }
}

int main()
{
//     foreach (s; MainWindow.ClassDef.signalList)
//         writeln("signal: ", s);
//     foreach (s; MainWindow.ClassDef.slotList)
//         writeln("slot: ", s);
//     foreach (s; MainWindow.ClassDef.methodList)
//         writeln("method: ", s);
//
//     writeln("\n=== genMetaStringData() ===\n", MainWindow.ClassDef.genMetaStringData(), "\n\n");
//     writeln("\n=== genMetaDataArray() ===\n", MainWindow.ClassDef.genMetaDataArray(), "\n\n");
//     writeln("\n=== genStaticMetaCallBody() ===\n", MainWindow.ClassDef.genStaticMetaCallBody(), "\n\n");
//     writeln("\n=== genMetaCastBody() ===\n", MainWindow.ClassDef.genMetaCastBody(), "\n\n");
//     writeln("\n=== genMetaCallBody() ===\n", MainWindow.ClassDef.genMetaCallBody(), "\n\n");

    auto app = new QApplication(Runtime.cArgs.argc, Runtime.cArgs.argv);

    auto CString = QString("Calypso");
    auto AEString = QString("Example App");
    app.setOrganizationName(CString);
    app.setApplicationName(AEString);

    auto mainWin = new MainWindow;
    auto testRecv = new TestReceiver;

    connect2!(MainWindow.testSignalNew, TestReceiver.recvNew)(mainWin, testRecv);
    connect2!(MainWindow.testSignalOpen, TestReceiver.recvOpen)(mainWin, testRecv);

    mainWin.show();

    return app.exec();
}
