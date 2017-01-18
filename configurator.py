# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'untitled.ui'
#
# Created by: PyQt5 UI code generator 5.7.1
#
# WARNING! All changes made in this file will be lost!
# :runtime optwin.vim
# list of options available in src/nvim/options.lua

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
#     ]
#     for c in candidates:
#         if os.path.isfile(c):
#             return True, c

#     return False,

prefix = 'checkBox_'
saveto = 'init.generated.vim'


class ConfigViewer(Ui_Dialog):

    def __init__(self, dialog):
        # super(ImageViewer, self).__init__()
        Ui_Dialog.__init__(self)
        self.setupUi(dialog)

        # Connect up the buttons.
        # self.number.clicked.connect(self.setnumber)
        # self.okButton.clicked.connect(self.accept)
        # self.cancelButton.clicked.connect(self.reject)

        # path=os.environ["NVIM_LISTEN_ADDRESS"]
        # https://github.com/neovim/python-client/issues/124
        # launch a headless nvim .
        self.nvim = neovim.attach('child', argv=["/usr/bin/env", "nvim", "--embed"])


    def nvim_cmd(self, cmd):
        """
        TODO use nvim.eval ?
        """
        try:
            log.debug("nvim attached")
            self.nvim.command('let oldwin = winnr()') 
            return res
        except Exception as e:
                log.error("Exception %s" % e)
                return False


    def getconfigfilename(self):
        """
        """
#         os.path.join(os.environ['XDG_CONFIG_HOME'], 'nvim', 'init.vim')
        return self.nvim.command_output('echo $MYVIMRC').strip('\n')

    def get_bool_options(self):
        opts = []
        for m in inspect.getmembers(self, lambda a:not(inspect.isroutine(a))):
            # print("variable", m)
            if m[0].startswith(prefix):
                opts.append(m[0][len(prefix):])
        return opts

    def loadconfig(self, filename):
        """
        use nvim_(win)get_option ?
        """
        opts = self.get_bool_options() # self.nvim.options
        # checkboxers can be tristate
        # print("STATUSLINE=", self.nvim.options['statusline'])
        # print("VISUALBELL=", self.nvim.options['visualbell'])
        for optname in opts:
    # can't work the way nvim is designed
        # for name, opt in opts.items():

            print("opt %s=%s" % (optname, self.nvim.options[optname]))
            widget = getattr(self, prefix + optname) # , self.nvim.options[optname])
            widget.setChecked(self.nvim.options[optname])

# vim.current.window.options['colorcolumn'] = '4,3'
        # print("VISUALBELL %r" % opts)
        # ("nvim_get_option")
        # self.nvim.command()
        # print("RESULT=", self.nvim.command_output('echo $MYVIMRC'))
                # vars['MYVIMRC'])
        # self.nvim.
        # TODO dir(self)
        # for var in ['check']:
        #     if var.__name__.startswith('checkbox_'):
        #         print(var.__name__ + " recognized !")
        # ask nvim process
        # with open(filename) as fd:
        #     lines = fd.readlines()

    def saveconfig(self, filename):
        """
        1. Writes into init.generated.vim 
        2. Checks that init.vim 
        """
        # save our own version
        # saveto
        config = self.getconfigfilename()
        config_dir = os.path.dirname(config)
        generated_cfg_fname = os.path.join(config_dir, saveto)
        log.info("Saving config into %s" % (generated_cfg_fname))
        with open(generated_cfg_fname, "w+") as fd:
            # TODO use inspect
            # for all booleans
            # write set (no)option
            for m in inspect.getmembers(self, lambda a: not(inspect.isroutine(a))):
                print("variable", m)
                if m[0].startswith('checkBox'):
                    # print("boolean")
                    print("name=", m[0][len(prefix):])
                    fd.write("set %s%s\n" % ("" if m[1].isChecked() else "no", m[0][len(prefix):]))

        import re

        addition = "runtime init.generated.vim"
        textfile = open(config, 'r')
        matches = []
        reg = re.compile("%s" % addition)
        text = textfile.read()
        textfile.close()
        matches = reg.findall(text)
        print("matches=", matches)
        if len(matches) == 0:
            textfile = open(config, 'a+')
            textfile.write(addition + "\n")

        print ("SAVED")
        # then append all that is after

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    dialog = QtWidgets.QDialog()

    t = ConfigViewer(dialog)

    t.loadconfig("fake")
    # imageViewer.main()
    dialog.show()
    ret = app.exec_()
    print(ret)
    # TODO act differently upon OK/Cancel
    t.saveconfig("fake")
    sys.exit(ret)
