#include <iostream>
#include <QtCore/QtCore>
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QAction>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QToolBar>

namespace demo {

    // D seems to have trouble with more than seven params for the constructor
    // so use a struct to pass extra params, if needed
    struct CPPactions {
	QAction *saveAct, *saveAsAct, *exitAct, *aboutAct, *aboutQtAct;
    };

    class DLangMainWindow : QMainWindow {
	Q_OBJECT
	
    public:
	DLangMainWindow(QPlainTextEdit*, QAction*, QAction*, QAction*, QAction*, QAction*, struct CPPactions*);
    public slots:
	virtual void DLangNewFile();
	virtual void DLangOpenFile();
	virtual void DLangSaveFile();
	
	virtual void DLangSaveAsFile();
	virtual void DLangAboutFile();
	
	virtual void DLangContentsChanged();
    protected:
	QPlainTextEdit      *textEdit;
	QMenu               *fileMenu;
	QAction             *copyAct, *cutAct, *pasteAct, *newAct, *openAct, *saveAct, \
	                    *saveAsAct, *exitAct, *aboutAct, *aboutQtAct;
    };

}
