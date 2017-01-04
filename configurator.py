# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'untitled.ui'
#
# Created by: PyQt5 UI code generator 5.7.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets
import sys
import neovim
import logging
import os
import inspect

from configuratorUI import Ui_Dialog

log = logging.getLogger(__name__)
# log.setLevel(logging.INFO)
log.setLevel(logging.DEBUG)

log.addHandler(logging.FileHandler(os.path.join(os.getenv("HOME", ""), "nvimconfigurator.log"), delay=False))


# QtGui.QMainWindow, 

# def find_vimrc():
#     """
#     TODO use command instead
#     returns success, filename
#     """
#     candidates = [
#         os.path.join(os.environ['XDG_CONFIG_HOME'], 'nvim', 'init.vim')
#     ]
#     for c in candidates:
#         if os.path.isfile(c):
#             return True, c

#     return False,




class ConfigViewer(Ui_Dialog):

    def __init__(self, dialog):
        # super(ImageViewer, self).__init__()
        Ui_Dialog.__init__(self)
        self.setupUi(dialog)

        # self.bools = {
        #     'number':
        #     'termguicolors':
        # }

        # Connect up the buttons.
        self.number.clicked.connect(self.setnumber)
        # self.okButton.clicked.connect(self.accept)
        # self.cancelButton.clicked.connect(self.reject)

        # path=os.environ["NVIM_LISTEN_ADDRESS"]
        # https://github.com/neovim/python-client/issues/124
        # launch a headless nvim .
        # TODO lancer une instance de nvim en specifiant socket ?
        # can get it from running instance with
        # echo v:servername

        path_to_socket="/tmp/nvimd4Na8G/0"
        self.nvim = neovim.attach('socket', path=path_to_socket)

    def nvim_cmd(self, cmd):
        """
        TODO use nvim.eval ?
        """
        try:
            log.debug("nvim attached")
            self.nvim.command('let oldwin = winnr()') 
            # nvim.command('wincmd ' + direction)
            # res = nvim.eval('oldwin != winnr()')
            # log.debug("Result of command %d" % res)
            return res
        except Exception as e:
                log.error("Exception %s" % e)
                return False

    def setnumber(self, val):
        print("value", val)
    # def main(self):
    #     self.show()

    def loadconfig(self, filename):
        """
        use nvim_get_option ?

        """
        # TODO dir(self)
        # for var in ['check']:
        #     if var.__name__.startswith('checkbox_'):
        #         print(var.__name__ + " recognized !")
        # ask nvim process
        # with open(filename) as fd:
        #     lines = fd.readlines()

    def saveconfig(self, filename):
        # save our own version
        with open(filename, "w+") as fd:
            # TODO use inspect
            # for all booleans
            # write set (no)option
            for m in inspect.getmembers(self, lambda a:not(inspect.isroutine(a))):
                print("variable", m)
                if(m[0].startswith('checkBox'))
                
            # fd.write()

        # then append all that is after

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    dialog = QtWidgets.QDialog()

    t = ConfigViewer(dialog)

    t.loadconfig("fake")
    t.saveconfig("fake")
    # imageViewer.main()
    dialog.show()

    sys.exit(app.exec_())
