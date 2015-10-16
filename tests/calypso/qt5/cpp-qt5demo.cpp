#include "cpp-qt5demo.hpp"

namespace demo {


    DLangMainWindow::DLangMainWindow(QPlainTextEdit *DTextEdit, QAction *DCopyAct, QAction *DCutAct, \
				     QAction *DPasteAct, QAction *DNewAct, QAction *DOpenAct, struct CPPactions *Dactions) {
	fileMenu = NULL;
	textEdit = DTextEdit;
	newAct = DNewAct;
	copyAct = DCopyAct;
	cutAct = DCutAct;
	pasteAct = DPasteAct;
	openAct = DOpenAct;
	
	saveAct = Dactions->saveAct;
	saveAsAct = Dactions->saveAsAct;
	exitAct = Dactions->exitAct;
	aboutAct = Dactions->aboutAct;
	aboutQtAct = Dactions->aboutQtAct;
	
	
	connect((QObject*)textEdit->document(), SIGNAL(contentsChanged()), (QObject*)this, SLOT(DLangContentsChanged()));
	connect((QObject*)textEdit, SIGNAL(copyAvailable(bool)), cutAct, SLOT(setEnabled(bool)));
	connect((QObject*)textEdit, SIGNAL(copyAvailable(bool)), copyAct, SLOT(setEnabled(bool)));
	
	connect((QObject*)copyAct, SIGNAL(triggered()), textEdit, SLOT(copy()));
	connect((QObject*)cutAct, SIGNAL(triggered()), textEdit, SLOT(cut()));
	connect((QObject*)pasteAct, SIGNAL(triggered()), textEdit, SLOT(paste()));
	
	connect((QObject*)newAct, SIGNAL(triggered()), this, SLOT(DLangNewFile()));
	connect((QObject*)openAct, SIGNAL(triggered()), this, SLOT(DLangOpenFile()));
	connect((QObject*)saveAct, SIGNAL(triggered()), this, SLOT(DLangSaveFile()));
	connect((QObject*)saveAsAct, SIGNAL(triggered()), this, SLOT(DLangSaveAsFile()));
	connect((QObject*)aboutAct, SIGNAL(triggered()), this, SLOT(DLangAboutFile()));
	
	connect((QObject*)exitAct, SIGNAL(triggered()), this, SLOT(close()));
	connect((QObject*)aboutQtAct, SIGNAL(triggered()), qApp, SLOT(aboutQt()));
    }
    
    void DLangMainWindow::DLangContentsChanged() {
	// This is commented out because if triggers on app load every time??
	//std::cout << "This content slot should be overriden in your D code." << std::endl;
    }
    
    
    void DLangMainWindow::DLangSaveAsFile() {
	std::cout << "This 'save as' file slot should be overriden in your D code." << std::endl;
    }
    void DLangMainWindow::DLangAboutFile() {
	std::cout << "This about file slot should be overriden in your D code." << std::endl;
    }
    
    
    void DLangMainWindow::DLangNewFile() {
	std::cout << "This new file slot should be overriden in your D code." << std::endl;
    }
    void DLangMainWindow::DLangOpenFile() {
	std::cout << "This open file slot should be overriden in your D code." << std::endl;
    }
    void DLangMainWindow::DLangSaveFile() {
	std::cout << "This save file slot should be overriden in your D code." << std::endl;
    }

}
