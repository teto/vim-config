# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'untitled.ui'
#
# Created by: PyQt5 UI code generator 5.7
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(400, 300)
        self.buttonBox = QtWidgets.QDialogButtonBox(Dialog)
        self.buttonBox.setGeometry(QtCore.QRect(30, 260, 341, 32))
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.tabWidget = QtWidgets.QTabWidget(Dialog)
        self.tabWidget.setGeometry(QtCore.QRect(10, 10, 371, 251))
        self.tabWidget.setObjectName("tabWidget")
        self.plug = QtWidgets.QWidget()
        self.plug.setObjectName("plug")
        self.checkBox_termguicolors = QtWidgets.QCheckBox(self.plug)
        self.checkBox_termguicolors.setGeometry(QtCore.QRect(30, 120, 141, 20))
        self.checkBox_termguicolors.setObjectName("checkBox_termguicolors")
        self.checkBox_visualbell = QtWidgets.QCheckBox(self.plug)
        self.checkBox_visualbell.setGeometry(QtCore.QRect(30, 90, 141, 20))
        self.checkBox_visualbell.setObjectName("checkBox_visualbell")
        self.pushButton = QtWidgets.QPushButton(self.plug)
        self.pushButton.setGeometry(QtCore.QRect(200, 160, 121, 20))
        self.pushButton.setObjectName("pushButton")
        self.pushButton_2 = QtWidgets.QPushButton(self.plug)
        self.pushButton_2.setGeometry(QtCore.QRect(200, 190, 121, 20))
        self.pushButton_2.setObjectName("pushButton_2")
        self.comboBox = QtWidgets.QComboBox(self.plug)
        self.comboBox.setGeometry(QtCore.QRect(30, 30, 141, 22))
        self.comboBox.setObjectName("comboBox")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.tabWidget.addTab(self.plug, "")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.tabWidget.addTab(self.tab_2, "")

        self.retranslateUi(Dialog)
        self.tabWidget.setCurrentIndex(0)
        self.buttonBox.accepted.connect(Dialog.accept)
        self.buttonBox.rejected.connect(Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.checkBox_termguicolors.setText(_translate("Dialog", "Enable true colors"))
        self.checkBox_visualbell.setToolTip(_translate("Dialog", "hello world"))
        self.checkBox_visualbell.setText(_translate("Dialog", "Enable visualbell"))
        self.pushButton.setText(_translate("Dialog", "Upgrade vim-plug"))
        self.pushButton_2.setText(_translate("Dialog", "Upgrade plugins"))
        self.comboBox.setItemText(0, _translate("Dialog", "No completion"))
        self.comboBox.setItemText(1, _translate("Dialog", "YouCompleteMe completion"))
        self.comboBox.setItemText(2, _translate("Dialog", "Rtags completion"))
        self.comboBox.setItemText(3, _translate("Dialog", "Deoplete Completion"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.plug), _translate("Dialog", "Plugins"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), _translate("Dialog", "Tab 2"))

