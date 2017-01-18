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
        self.buttonBox.setGeometry(QtCore.QRect(30, 240, 341, 32))
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.checkBox_termguicolors = QtWidgets.QCheckBox(Dialog)
        self.checkBox_termguicolors.setGeometry(QtCore.QRect(20, 60, 141, 20))
        self.checkBox_termguicolors.setObjectName("checkBox_termguicolors")
        self.checkBox_visualbell = QtWidgets.QCheckBox(Dialog)
        self.checkBox_visualbell.setGeometry(QtCore.QRect(20, 80, 141, 20))
        self.checkBox_visualbell.setObjectName("checkBox_visualbell")

        self.retranslateUi(Dialog)
        self.buttonBox.accepted.connect(Dialog.accept)
        self.buttonBox.rejected.connect(Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.checkBox_termguicolors.setText(_translate("Dialog", "Enable true colors"))
        self.checkBox_visualbell.setText(_translate("Dialog", "Enable visualbell"))

