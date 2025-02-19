#
# This is an example keymap for Jwno's :ui-hint command.
#
# To use it, place this file alongside your config file,
# and add these code in your config:
#
#     (import ui-hint-keymap)
#     # Keys used in hint labels. This can be set to your own prefered keys.
#     # Only alphabetic keys are supported though.
#     (def hint-key-list "fjghdksla")
#     (ui-hint-keymap/define-ui-hint-keys your-keymap hint-key-list jwno/context)
#     ...
#     (:set-keymap (in jwno/context :key-manager) your-keymap)
#
# It will enable the ui-hint feature, and make some ui-hint commands
# available under the `Win + Q` prefix.
#
# When one of the :ui-hint commands is called, labels will be shown on top of
# relevant UI elements. Type the letters shown in its label to select a UI
# element. To cancel the selection, press Esc.
#
# Due to some limitations of the current implementation, calling the :ui-hint
# command on a complex UI (e.g. a browser showing a complex web page) is
# reeeeaaaally slow. You may need to wait a bit for the labels to appear.
#


# For the UIA_* constants
(use jw32/_uiautomation)

# For the ui-hint feature
(import jwno/ui-hint)


(defn define-ui-hint-keys [keymap hint-key-list context]
  (def ui-hint (ui-hint/ui-hint context))
  (:enable ui-hint)

  (:define-key keymap
               "Win + Q A"
               [:ui-hint hint-key-list]
               "Show all focusable and invokable elements")
  (:define-key keymap
               "Win + Q B"
               [:ui-hint hint-key-list [:or
                                        [:property UIA_ControlTypePropertyId UIA_ButtonControlTypeId]
                                        [:property UIA_ControlTypePropertyId UIA_CheckBoxControlTypeId]]]
               "Show all buttons")
  (:define-key keymap
               "Win + Q C"
               [:ui-hint hint-key-list nil :click]
               "Show all focusable and invokable elements, and click on the selected one")
  (:define-key keymap
               "Win + Q D"
               [:ui-hint hint-key-list nil :double-click]
               "Show all focusable and invokable elements, and double-click on the selected one")
  (:define-key keymap
               "Win + Q E"
               [:ui-hint hint-key-list [:and
                                        [:or
                                         [:property UIA_ControlTypePropertyId UIA_EditControlTypeId]
                                         [:property UIA_ControlTypePropertyId UIA_ComboBoxControlTypeId]]
                                        [:property UIA_IsKeyboardFocusablePropertyId true]]]
               "Show all editable fields")
  (:define-key keymap
               "Win + Q F"
               [:ui-hint hint-key-list [:property UIA_IsKeyboardFocusablePropertyId true] :focus]
               "Show all focusable elements, and set focus to the selected one")
  (:define-key keymap
               "Win + Q I"
               [:ui-hint hint-key-list [:property UIA_ControlTypePropertyId UIA_ListItemControlTypeId]]
               "Show all list items")
  (:define-key keymap
               "Win + Q L"
               [:ui-hint hint-key-list [:property UIA_ControlTypePropertyId UIA_HyperlinkControlTypeId]]
               "Show all hyperlinks")
  (:define-key keymap
               "Win + Q M"
               [:ui-hint hint-key-list nil :middle-click]
               "Show all focusable and invokable elements, and middle-click on the selected one")
  (:define-key keymap
               "Win + Q Shift + M"
               [:ui-hint hint-key-list nil :move-cursor]
               "Show all focusable and invokable elements, and move the cursor to selected one")
  (:define-key keymap
               "Win + Q R"
               [:ui-hint hint-key-list nil :right-click]
               "Show all focusable and invokable elements, and right-click on the selected one")
  (:define-key keymap
               "Win + Q T"
               [:ui-hint hint-key-list [:property UIA_ControlTypePropertyId UIA_TreeItemControlTypeId]]
               "Show all tree items")

  (:define-key keymap
               "Win + Q Esc"
               :nop
               "Cancel")
  (:define-key keymap
               "Win + Q Enter"
               :nop
               "Cancel")

  [keymap ui-hint])
