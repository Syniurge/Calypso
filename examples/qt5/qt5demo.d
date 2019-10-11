/**
 * D adaptation of the Qt5 tutorial at: http://doc.qt.io/qt-5/qtwidgets-mainwindows-application-example.html
 *
 * Build with:
 *   $ ldc2 -relocation-model=pic -v -wi -cpp-args "-std=c++11 -D_REENTRANT -fPIE -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -I/pathto/Qt/5.x/gcc_64/mkspecs/linux-g++ -I/pathto/Qt/5.x/gcc_64/include -I/pathto/Qt/5.x/gcc_64/include/QtWidgets -I/pathto/Qt/5.x/gcc_64/include/QtGui -I/pathto/Qt/5.x/gcc_64/include/QtCore -I/pathto/Qt/5.x/gcc_64/include/QtCore/5.x.y -I/pathto/Qt/5.x/gcc_64/include/QtCore/5.x.y/QtCore" -L-L/pathto/Qt/5.x/gcc_64/lib -L-rpath=/pathto/Qt/5.x/gcc_64/lib -L-lQt5Widgets -L-lQt5Gui -L-lQt5Core -L-lGL qt5demo.d moc/package.d moc/moc_.d moc/types.d
 */

// D imports
import moc;
import core.runtime;
import std.stdio, std.conv;

// C++ Qt imports
pragma (cppmap, "<QtWidgets>");
import (C++) Qt, *;

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

        setCurrentFile("");
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
            setCurrentFile("");
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
        QMessageBox.about(this, "About Application",
                        "This <b>Application</b> " ~
                        "example demonstrates how to write modern GUI applications in D " ~
                        "using QT and Calypso, with a menu bar, toolbars, and a status bar.");
    }

    void documentWasModified()
    {
        setWindowModified(textEdit.document().isModified());
    }
} // end of extern(C++)  @slots

    void createActions()
    {
        alias StandardKey = QKeySequence.StandardKey;

        auto newIcon = QIcon.fromTheme( "document-new", QIcon( "images/new.png" ) );
        newAct = new QAction( newIcon, "&New", this );
        newAct.setShortcuts( StandardKey.New );
        newAct.setStatusTip( "Create a new File" );
        connect2!(QAction.triggered, newFile)(newAct, this);

        auto openIcon = QIcon.fromTheme( "document-open", QIcon( "images/open.png" ) );
        openAct = new QAction( openIcon, "&Open", this );
        openAct.setShortcuts( StandardKey.Open );
        openAct.setStatusTip( "Open an existing file" );
        connect2!(QAction.triggered, open)(openAct, this);

        auto saveIcon = QIcon.fromTheme( "document-save", QIcon( "images/save.png" ) );
        saveAct = new QAction( saveIcon, "&Save", this );
        saveAct.setShortcuts( StandardKey.Save );
        saveAct.setStatusTip( "Save the document to disk" );
        connect2!(QAction.triggered, save)(saveAct, this);

        auto saveAsIcon = QIcon.fromTheme( "document-save-as" );
        saveAsAct = new QAction( saveAsIcon, "Save &As...", this );
        saveAsAct.setShortcuts( StandardKey.SaveAs );
        saveAsAct.setStatusTip( "Save the document under a new name" );
        connect2!(QAction.triggered, saveAs)(saveAsAct, this);

        auto exitIcon = QIcon.fromTheme( "application-exit" );
        exitAct = new QAction( exitIcon, "E&xit", this );
        exitAct.setShortcuts( StandardKey.Close );
        exitAct.setStatusTip( "Exit application" );
        connect2!(QAction.triggered, close)(exitAct, this);

        // Edit Menu actions
        auto copyIcon = QIcon.fromTheme( "edit-copy", QIcon( "images/copy.png" ) );
        copyAct = new QAction( copyIcon, "&Copy", this );
        copyAct.setShortcuts( StandardKey.Copy );
        copyAct.setStatusTip( "Copy text and save it to clipboard" );
        connect2!(QAction.triggered, QPlainTextEdit.copy)(copyAct, textEdit);

        auto cutIcon = QIcon.fromTheme( "edit-cut", QIcon( "images/cut.png" ) );
        cutAct = new QAction( cutIcon, "&Cut", this );
        cutAct.setShortcuts(StandardKey.Cut);
        cutAct.setStatusTip( "Cut text and save it to clipboard" );
        connect2!(QAction.triggered, QPlainTextEdit.cut)(cutAct, textEdit);

        auto pasteIcon = QIcon.fromTheme( "edit-paste", QIcon( "images/paste.png" ) );
        pasteAct = new QAction( pasteIcon, "&Paste", this );
        pasteAct.setShortcuts( StandardKey.Paste );
        pasteAct.setStatusTip( "Paste text from clipboard" );
        connect2!(QAction.triggered, QPlainTextEdit.paste)(pasteAct, textEdit);

        // Help Menu actions
        aboutAct = new QAction( "&About", this );
        aboutAct.setStatusTip( "About the Application" );
        connect2!(QAction.triggered, about)(aboutAct, this);

        aboutQtAct = new QAction( "About &Qt", this );
        aboutQtAct.setStatusTip( "Show the Qt Library's About Box" );
        connect2!(QAction.triggered, QApplication.aboutQt)(aboutQtAct, QCoreApplication.instance());

        copyAct.setEnabled(false);
        cutAct.setEnabled(false);
        connect2!(QPlainTextEdit.copyAvailable, QAction.setEnabled)(textEdit, cutAct);
        connect2!(QPlainTextEdit.copyAvailable, QAction.setEnabled)(textEdit, copyAct);
    }

    void createMenus()
    {
        fileMenu = menuBar().addMenu( "&File" );

        fileMenu.QWidget.addAction(newAct);
        fileMenu.QWidget.addAction(openAct);
        fileMenu.QWidget.addAction(saveAct);
        fileMenu.QWidget.addAction(saveAsAct);
        fileMenu.addSeparator();
        fileMenu.QWidget.addAction(exitAct);

        editMenu = menuBar().addMenu( "&Edit" );
        editMenu.QWidget.addAction(cutAct);
        editMenu.QWidget.addAction(copyAct);
        editMenu.QWidget.addAction(pasteAct);

        menuBar.addSeparator();

        helpMenu = menuBar().addMenu( "&Help" );
        helpMenu.QWidget.addAction(aboutAct);
        helpMenu.QWidget.addAction(aboutQtAct);
    }

    void createToolBars()
    {
        fileToolBar = addToolBar( "File" );

        fileToolBar.QWidget.addAction(newAct);
        fileToolBar.QWidget.addAction(openAct);
        fileToolBar.QWidget.addAction(saveAct);

        editToolBar = addToolBar( "Edit" );

        editToolBar.QWidget.addAction(cutAct);
        editToolBar.QWidget.addAction(copyAct);
        editToolBar.QWidget.addAction(pasteAct);
    }

    void createStatusBar()
    {
        statusBar().showMessage( "Ready" );
    }

    void readSettings()
    {
        QSettings settings = QSettings( "Trolltech", "Application Example" );

        QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
        QSize size = settings.value("size", QSize(400, 400)).toSize();
        resize(size);
        move(pos);
    }

    void writeSettings()
    {
        QSettings settings = QSettings( "Trolltech", "Application Example" );
        settings.setValue("size", size());
    }

    bool maybeSave()
    {
        if (textEdit.document().isModified())
        {
            alias StandardButton = QMessageBox.StandardButton;
            StandardButton ret;

            auto flags = QFlags!(QMessageBox.StandardButton)(StandardButton.Save | StandardButton.Discard | StandardButton.Cancel);
            ret = cast(StandardButton) QMessageBox.warning(this, "Application",
                                "The document has been modified, Do you want to save?", flags);

            if (ret == StandardButton.Save)
                return save();
            else if (ret == StandardButton.Cancel)
                return false;
        }
        return true;
    }

    void loadFile(const scope ref QString fileName)
    {
        auto file = QFile(fileName);

        alias omf = QIODevice.OpenModeFlag;
        auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.ReadOnly | omf.Text);

        if (!file.open(flags))
        {
            auto d_WarningString = "Cannot read file " ~
                    to!string(fileName.toUtf8.data()) ~ ": \n" ~
                    to!string(file.errorString().toUtf8.data());

            QMessageBox.warning( this, "Application", d_WarningString.ptr );
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
        statusBar().showMessage( "File Loaded", 2000 );
    }

    bool saveFile(const scope ref QString fileName)
    {
        QString str = textEdit.toPlainText();
        auto text = to!string(str.toUtf8.data);

        auto file = QFile(fileName);

        alias omf = QIODevice.OpenModeFlag;
        auto flags = QFlags!(QIODevice.OpenModeFlag)(omf.WriteOnly | omf.Text);

        if (!file.open(flags)) {
            auto d_WarningString = "Cannot write file " ~
                    to!string(fileName.toUtf8.data) ~ ": \n" ~
                    to!string(file.errorString().toUtf8.data);

            QMessageBox.warning( this, "Application", d_WarningString.ptr );
            return false;
        }

        auto out_ = QTextStream(&file);

        static if (!HAS_QT_NO_CURSOR)
        {
            auto cursor = QCursor(CursorShape.WaitCursor);
            QApplication.setOverrideCursor(cursor);
        }

        out_ << textEdit.toPlainText();

        static if (!HAS_QT_NO_CURSOR)
            QApplication.restoreOverrideCursor();

        setCurrentFile(fileName);
        statusBar().showMessage( "File saved", 2000);
        return true;
    }

    void setCurrentFile(const scope ref QString fileName)
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

    auto app = QApplication(Runtime.cArgs.argc, Runtime.cArgs.argv);

    app.setOrganizationName( "Calypso" );
    app.setApplicationName( "Example App" );

    auto mainWin = new MainWindow;
    auto testRecv = new TestReceiver;

    connect2!(MainWindow.testSignalNew, TestReceiver.recvNew)(mainWin, testRecv);
    connect2!(MainWindow.testSignalOpen, TestReceiver.recvOpen)(mainWin, testRecv);

    mainWin.show();

    return app.exec();
}
