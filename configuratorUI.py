# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'untitled.ui'
#
# Created by: PyQt5 UI code generator 5.7
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        print("SETUP UI")
        Dialog.setObjectName("Dialog")
        Dialog.resize(400, 300)
        self.buttonBox = QtWidgets.QDialogButtonBox(Dialog)
        self.buttonBox.setGeometry(QtCore.QRect(30, 240, 341, 32))
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.number = QtWidgets.QCheckBox(Dialog)
        self.number.setGeometry(QtCore.QRect(20, 20, 121, 20))
        self.number.setObjectName("number")
        self.__checkBox_cursorline = QtWidgets.QCheckBox(Dialog)
        self.__checkBox_cursorline.setGeometry(QtCore.QRect(20, 40, 141, 20))
        self.__checkBox_cursorline.setObjectName("__checkBox_cursorline")
        self.checkBox_termguicolors = QtWidgets.QCheckBox(Dialog)
        self.checkBox_termguicolors.setGeometry(QtCore.QRect(20, 60, 141, 20))
        self.checkBox_termguicolors.setObjectName("checkBox_termguicolors")
        self.checkBox_visualbell = QtWidgets.QCheckBox(Dialog)
        self.checkBox_visualbell.setGeometry(QtCore.QRect(20, 80, 141, 20))
        self.checkBox_visualbell.setObjectName("checkBox_visualbell")
        self.comboBox = QtWidgets.QComboBox(Dialog)
        self.comboBox.setGeometry(QtCore.QRect(30, 120, 79, 22))
        self.comboBox.setObjectName("comboBox")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.comboBox.addItem("")

        self.retranslateUi(Dialog)
        self.buttonBox.accepted.connect(Dialog.accept)
        self.buttonBox.rejected.connect(Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.number.setText(_translate("Dialog", "Enable line number"))
        self.__checkBox_cursorline.setText(_translate("Dialog", "Show current line"))
        self.checkBox_termguicolors.setText(_translate("Dialog", "Enable true colors"))
        self.checkBox_visualbell.setText(_translate("Dialog", "Enable visualbell"))
        self.comboBox.setItemText(0, _translate("Dialog", "No inccommand"))
        self.comboBox.setItemText(1, _translate("Dialog", "Show subtitution in a split"))
        self.comboBox.setItemText(2, _translate("Dialog", "Not in a split"))

