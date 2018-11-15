(define-module (Gtk)
  #:use-module (gi)
  #:export(
           ;; Functions
           GtkAboutDialog-add-credit-section
           GtkAboutDialog-get-artists
           GtkAboutDialog-get-authors
           GtkAboutDialog-get-comments
           GtkAboutDialog-get-copyright
           GtkAboutDialog-get-documenters
           GtkAboutDialog-get-license
           GtkAboutDialog-get-license-type
           GtkAboutDialog-get-logo
           GtkAboutDialog-get-logo-icon-name
           GtkAboutDialog-get-program-name
           GtkAboutDialog-get-translator-credits
           GtkAboutDialog-get-version
           GtkAboutDialog-get-website
           GtkAboutDialog-get-website-label
           GtkAboutDialog-get-wrap-license?
           GtkAboutDialog-new
           GtkAboutDialog-set-artists
           GtkAboutDialog-set-authors
           GtkAboutDialog-set-comments
           GtkAboutDialog-set-copyright
           GtkAboutDialog-set-documenters
           GtkAboutDialog-set-license
           GtkAboutDialog-set-license-type
           GtkAboutDialog-set-logo
           GtkAboutDialog-set-logo-icon-name
           GtkAboutDialog-set-program-name
           GtkAboutDialog-set-translator-credits
           GtkAboutDialog-set-version
           GtkAboutDialog-set-website
           GtkAboutDialog-set-website-label
           GtkAboutDialog-set-wrap-license
           GtkAccelGroup-activate?
           GtkAccelGroup-connect
           GtkAccelGroup-connect-by-path
           GtkAccelGroup-disconnect-key?
           GtkAccelGroup-disconnect?
           GtkAccelGroup-find
           GtkAccelGroup-from-accel-closure
           GtkAccelGroup-get-is-locked?
           GtkAccelGroup-get-modifier-mask
           GtkAccelGroup-lock
           GtkAccelGroup-new
           GtkAccelGroup-query
           GtkAccelGroup-unlock
           GtkAccelLabel-get-accel
           GtkAccelLabel-get-accel-widget
           GtkAccelLabel-get-accel-width
           GtkAccelLabel-new
           GtkAccelLabel-refetch?
           GtkAccelLabel-set-accel
           GtkAccelLabel-set-accel-closure
           GtkAccelLabel-set-accel-widget
           GtkAccelMap-add-entry
           GtkAccelMap-add-filter
           GtkAccelMap-change-entry?
           GtkAccelMap-foreach
           GtkAccelMap-foreach-unfiltered
           GtkAccelMap-get
           GtkAccelMap-load
           GtkAccelMap-load-fd
           GtkAccelMap-load-scanner
           GtkAccelMap-lock-path
           GtkAccelMap-lookup-entry?
           GtkAccelMap-save
           GtkAccelMap-save-fd
           GtkAccelMap-unlock-path
           GtkAccessible-connect-widget-destroyed
           GtkAccessible-get-widget
           GtkAccessible-set-widget
           GtkAction-activate
           GtkAction-block-activate
           GtkAction-connect-accelerator
           GtkAction-create-icon
           GtkAction-create-menu
           GtkAction-create-menu-item
           GtkAction-create-tool-item
           GtkAction-disconnect-accelerator
           GtkAction-get-accel-closure
           GtkAction-get-accel-path
           GtkAction-get-always-show-image?
           GtkAction-get-gicon
           GtkAction-get-icon-name
           GtkAction-get-is-important?
           GtkAction-get-label
           GtkAction-get-name
           GtkAction-get-proxies
           GtkAction-get-sensitive?
           GtkAction-get-short-label
           GtkAction-get-stock-id
           GtkAction-get-tooltip
           GtkAction-get-visible-horizontal?
           GtkAction-get-visible-vertical?
           GtkAction-get-visible?
           GtkAction-is-sensitive?
           GtkAction-is-visible?
           GtkAction-new
           GtkAction-set-accel-group
           GtkAction-set-accel-path
           GtkAction-set-always-show-image
           GtkAction-set-gicon
           GtkAction-set-icon-name
           GtkAction-set-is-important
           GtkAction-set-label
           GtkAction-set-sensitive
           GtkAction-set-short-label
           GtkAction-set-stock-id
           GtkAction-set-tooltip
           GtkAction-set-visible
           GtkAction-set-visible-horizontal
           GtkAction-set-visible-vertical
           GtkAction-unblock-activate
           GtkActionBar-get-center-widget
           GtkActionBar-new
           GtkActionBar-pack-end
           GtkActionBar-pack-start
           GtkActionBar-set-center-widget
           GtkActionGroup-add-action
           GtkActionGroup-add-action-with-accel
           GtkActionGroup-get-accel-group
           GtkActionGroup-get-action
           GtkActionGroup-get-name
           GtkActionGroup-get-sensitive?
           GtkActionGroup-get-visible?
           GtkActionGroup-list-actions
           GtkActionGroup-new
           GtkActionGroup-remove-action
           GtkActionGroup-set-accel-group
           GtkActionGroup-set-sensitive
           GtkActionGroup-set-translate-func
           GtkActionGroup-set-translation-domain
           GtkActionGroup-set-visible
           GtkActionGroup-translate-string
           GtkAdjustment-changed
           GtkAdjustment-clamp-page
           GtkAdjustment-configure
           GtkAdjustment-get-lower
           GtkAdjustment-get-minimum-increment
           GtkAdjustment-get-page-increment
           GtkAdjustment-get-page-size
           GtkAdjustment-get-step-increment
           GtkAdjustment-get-upper
           GtkAdjustment-get-value
           GtkAdjustment-new
           GtkAdjustment-set-lower
           GtkAdjustment-set-page-increment
           GtkAdjustment-set-page-size
           GtkAdjustment-set-step-increment
           GtkAdjustment-set-upper
           GtkAdjustment-set-value
           GtkAdjustment-value-changed
           GtkAlignment-get-padding
           GtkAlignment-new
           GtkAlignment-set
           GtkAlignment-set-padding
           GtkAppChooserButton-append-custom-item
           GtkAppChooserButton-append-separator
           GtkAppChooserButton-get-heading
           GtkAppChooserButton-get-show-default-item?
           GtkAppChooserButton-get-show-dialog-item?
           GtkAppChooserButton-new
           GtkAppChooserButton-set-active-custom-item
           GtkAppChooserButton-set-heading
           GtkAppChooserButton-set-show-default-item
           GtkAppChooserButton-set-show-dialog-item
           GtkAppChooserDialog-get-heading
           GtkAppChooserDialog-get-widget
           GtkAppChooserDialog-new
           GtkAppChooserDialog-new-for-content-type
           GtkAppChooserDialog-set-heading
           GtkAppChooserWidget-get-default-text
           GtkAppChooserWidget-get-show-all?
           GtkAppChooserWidget-get-show-default?
           GtkAppChooserWidget-get-show-fallback?
           GtkAppChooserWidget-get-show-other?
           GtkAppChooserWidget-get-show-recommended?
           GtkAppChooserWidget-new
           GtkAppChooserWidget-set-default-text
           GtkAppChooserWidget-set-show-all
           GtkAppChooserWidget-set-show-default
           GtkAppChooserWidget-set-show-fallback
           GtkAppChooserWidget-set-show-other
           GtkAppChooserWidget-set-show-recommended
           GtkApplication-add-accelerator
           GtkApplication-add-window
           GtkApplication-get-accels-for-action
           GtkApplication-get-actions-for-accel
           GtkApplication-get-active-window
           GtkApplication-get-app-menu
           GtkApplication-get-menu-by-id
           GtkApplication-get-menubar
           GtkApplication-get-window-by-id
           GtkApplication-get-windows
           GtkApplication-inhibit
           GtkApplication-is-inhibited?
           GtkApplication-list-action-descriptions
           GtkApplication-new
           GtkApplication-prefers-app-menu?
           GtkApplication-remove-accelerator
           GtkApplication-remove-window
           GtkApplication-set-accels-for-action
           GtkApplication-set-app-menu
           GtkApplication-set-menubar
           GtkApplication-uninhibit
           GtkApplicationWindow-get-help-overlay
           GtkApplicationWindow-get-id
           GtkApplicationWindow-get-show-menubar?
           GtkApplicationWindow-new
           GtkApplicationWindow-set-help-overlay
           GtkApplicationWindow-set-show-menubar
           GtkArrow-new
           GtkArrow-set
           GtkAspectFrame-new
           GtkAspectFrame-set
           GtkAssistant-add-action-widget
           GtkAssistant-append-page
           GtkAssistant-commit
           GtkAssistant-get-current-page
           GtkAssistant-get-n-pages
           GtkAssistant-get-nth-page
           GtkAssistant-get-page-complete?
           GtkAssistant-get-page-has-padding?
           GtkAssistant-get-page-header-image
           GtkAssistant-get-page-side-image
           GtkAssistant-get-page-title
           GtkAssistant-get-page-type
           GtkAssistant-insert-page
           GtkAssistant-new
           GtkAssistant-next-page
           GtkAssistant-prepend-page
           GtkAssistant-previous-page
           GtkAssistant-remove-action-widget
           GtkAssistant-remove-page
           GtkAssistant-set-current-page
           GtkAssistant-set-forward-page-func
           GtkAssistant-set-page-complete
           GtkAssistant-set-page-has-padding
           GtkAssistant-set-page-header-image
           GtkAssistant-set-page-side-image
           GtkAssistant-set-page-title
           GtkAssistant-set-page-type
           GtkAssistant-update-buttons-state
           GtkBin-get-child
           GtkBorder-copy
           GtkBorder-free
           GtkBorder-new
           GtkBox-get-baseline-position
           GtkBox-get-center-widget
           GtkBox-get-homogeneous?
           GtkBox-get-spacing
           GtkBox-new
           GtkBox-pack-end
           GtkBox-pack-start
           GtkBox-query-child-packing
           GtkBox-reorder-child
           GtkBox-set-baseline-position
           GtkBox-set-center-widget
           GtkBox-set-child-packing
           GtkBox-set-homogeneous
           GtkBox-set-spacing
           GtkBuilder-add-callback-symbol
           GtkBuilder-add-from-file
           GtkBuilder-add-from-resource
           GtkBuilder-add-from-string
           GtkBuilder-add-objects-from-file
           GtkBuilder-add-objects-from-resource
           GtkBuilder-add-objects-from-string
           GtkBuilder-connect-signals
           GtkBuilder-connect-signals-full
           GtkBuilder-expose-object
           GtkBuilder-extend-with-template
           GtkBuilder-get-application
           GtkBuilder-get-object
           GtkBuilder-get-objects
           GtkBuilder-get-translation-domain
           GtkBuilder-get-type-from-name
           GtkBuilder-new
           GtkBuilder-new-from-file
           GtkBuilder-new-from-resource
           GtkBuilder-new-from-string
           GtkBuilder-set-application
           GtkBuilder-set-translation-domain
           GtkBuilder-value-from-string-type?
           GtkBuilder-value-from-string?
           GtkButton-clicked
           GtkButton-enter
           GtkButton-get-alignment
           GtkButton-get-always-show-image?
           GtkButton-get-event-window
           GtkButton-get-focus-on-click?
           GtkButton-get-image
           GtkButton-get-image-position
           GtkButton-get-label
           GtkButton-get-relief
           GtkButton-get-use-stock?
           GtkButton-get-use-underline?
           GtkButton-leave
           GtkButton-new
           GtkButton-new-from-icon-name
           GtkButton-new-from-stock
           GtkButton-new-with-label
           GtkButton-new-with-mnemonic
           GtkButton-pressed
           GtkButton-released
           GtkButton-set-alignment
           GtkButton-set-always-show-image
           GtkButton-set-focus-on-click
           GtkButton-set-image
           GtkButton-set-image-position
           GtkButton-set-label
           GtkButton-set-relief
           GtkButton-set-use-stock
           GtkButton-set-use-underline
           GtkButtonBox-get-child-non-homogeneous?
           GtkButtonBox-get-child-secondary?
           GtkButtonBox-get-layout
           GtkButtonBox-new
           GtkButtonBox-set-child-non-homogeneous
           GtkButtonBox-set-child-secondary
           GtkButtonBox-set-layout
           GtkCalendar-clear-marks
           GtkCalendar-get-date
           GtkCalendar-get-day-is-marked?
           GtkCalendar-get-detail-height-rows
           GtkCalendar-get-detail-width-chars
           GtkCalendar-get-display-options
           GtkCalendar-mark-day
           GtkCalendar-new
           GtkCalendar-select-day
           GtkCalendar-select-month
           GtkCalendar-set-detail-func
           GtkCalendar-set-detail-height-rows
           GtkCalendar-set-detail-width-chars
           GtkCalendar-set-display-options
           GtkCalendar-unmark-day
           GtkCellArea-activate-cell?
           GtkCellArea-activate?
           GtkCellArea-add
           GtkCellArea-add-focus-sibling
           GtkCellArea-apply-attributes
           GtkCellArea-attribute-connect
           GtkCellArea-attribute-disconnect
           GtkCellArea-attribute-get-column
           GtkCellArea-cell-get-property
           GtkCellArea-cell-set-property
           GtkCellArea-copy-context
           GtkCellArea-create-context
           GtkCellArea-event
           GtkCellArea-focus?
           GtkCellArea-foreach
           GtkCellArea-foreach-alloc
           GtkCellArea-get-cell-allocation
           GtkCellArea-get-cell-at-position
           GtkCellArea-get-current-path-string
           GtkCellArea-get-edit-widget
           GtkCellArea-get-edited-cell
           GtkCellArea-get-focus-cell
           GtkCellArea-get-focus-from-sibling
           GtkCellArea-get-focus-siblings
           GtkCellArea-get-preferred-height
           GtkCellArea-get-preferred-height-for-width
           GtkCellArea-get-preferred-width
           GtkCellArea-get-preferred-width-for-height
           GtkCellArea-get-request-mode
           GtkCellArea-has-renderer?
           GtkCellArea-inner-cell-area
           GtkCellArea-is-activatable?
           GtkCellArea-is-focus-sibling?
           GtkCellArea-remove
           GtkCellArea-remove-focus-sibling
           GtkCellArea-render
           GtkCellArea-request-renderer
           GtkCellArea-set-focus-cell
           GtkCellArea-stop-editing
           GtkCellAreaBox-get-spacing
           GtkCellAreaBox-new
           GtkCellAreaBox-pack-end
           GtkCellAreaBox-pack-start
           GtkCellAreaBox-set-spacing
           GtkCellAreaContext-allocate
           GtkCellAreaContext-get-allocation
           GtkCellAreaContext-get-area
           GtkCellAreaContext-get-preferred-height
           GtkCellAreaContext-get-preferred-height-for-width
           GtkCellAreaContext-get-preferred-width
           GtkCellAreaContext-get-preferred-width-for-height
           GtkCellAreaContext-push-preferred-height
           GtkCellAreaContext-push-preferred-width
           GtkCellAreaContext-reset
           GtkCellRenderer-activate?
           GtkCellRenderer-get-aligned-area
           GtkCellRenderer-get-alignment
           GtkCellRenderer-get-fixed-size
           GtkCellRenderer-get-padding
           GtkCellRenderer-get-preferred-height
           GtkCellRenderer-get-preferred-height-for-width
           GtkCellRenderer-get-preferred-size
           GtkCellRenderer-get-preferred-width
           GtkCellRenderer-get-preferred-width-for-height
           GtkCellRenderer-get-request-mode
           GtkCellRenderer-get-sensitive?
           GtkCellRenderer-get-size
           GtkCellRenderer-get-state
           GtkCellRenderer-get-visible?
           GtkCellRenderer-is-activatable?
           GtkCellRenderer-render
           GtkCellRenderer-set-alignment
           GtkCellRenderer-set-fixed-size
           GtkCellRenderer-set-padding
           GtkCellRenderer-set-sensitive
           GtkCellRenderer-set-visible
           GtkCellRenderer-start-editing
           GtkCellRenderer-stop-editing
           GtkCellRendererAccel-new
           GtkCellRendererCombo-new
           GtkCellRendererPixbuf-new
           GtkCellRendererProgress-new
           GtkCellRendererSpin-new
           GtkCellRendererSpinner-new
           GtkCellRendererText-new
           GtkCellRendererText-set-fixed-height-from-font
           GtkCellRendererToggle-get-activatable?
           GtkCellRendererToggle-get-active?
           GtkCellRendererToggle-get-radio?
           GtkCellRendererToggle-new
           GtkCellRendererToggle-set-activatable
           GtkCellRendererToggle-set-active
           GtkCellRendererToggle-set-radio
           GtkCellView-get-displayed-row
           GtkCellView-get-draw-sensitive?
           GtkCellView-get-fit-model?
           GtkCellView-get-model
           GtkCellView-get-size-of-row?
           GtkCellView-new
           GtkCellView-new-with-context
           GtkCellView-new-with-markup
           GtkCellView-new-with-pixbuf
           GtkCellView-new-with-text
           GtkCellView-set-background-color
           GtkCellView-set-background-rgba
           GtkCellView-set-displayed-row
           GtkCellView-set-draw-sensitive
           GtkCellView-set-fit-model
           GtkCellView-set-model
           GtkCheckButton-new
           GtkCheckButton-new-with-label
           GtkCheckButton-new-with-mnemonic
           GtkCheckMenuItem-get-active?
           GtkCheckMenuItem-get-draw-as-radio?
           GtkCheckMenuItem-get-inconsistent?
           GtkCheckMenuItem-new
           GtkCheckMenuItem-new-with-label
           GtkCheckMenuItem-new-with-mnemonic
           GtkCheckMenuItem-set-active
           GtkCheckMenuItem-set-draw-as-radio
           GtkCheckMenuItem-set-inconsistent
           GtkCheckMenuItem-toggled
           GtkClipboard-clear
           GtkClipboard-get
           GtkClipboard-get-default
           GtkClipboard-get-display
           GtkClipboard-get-for-display
           GtkClipboard-get-owner
           GtkClipboard-request-contents
           GtkClipboard-request-image
           GtkClipboard-request-rich-text
           GtkClipboard-request-targets
           GtkClipboard-request-text
           GtkClipboard-request-uris
           GtkClipboard-set-can-store
           GtkClipboard-set-image
           GtkClipboard-set-text
           GtkClipboard-store
           GtkClipboard-wait-for-contents
           GtkClipboard-wait-for-image
           GtkClipboard-wait-for-rich-text
           GtkClipboard-wait-for-targets?
           GtkClipboard-wait-for-text
           GtkClipboard-wait-for-uris
           GtkClipboard-wait-is-image-available?
           GtkClipboard-wait-is-rich-text-available?
           GtkClipboard-wait-is-target-available?
           GtkClipboard-wait-is-text-available?
           GtkClipboard-wait-is-uris-available?
           GtkColorButton-get-alpha
           GtkColorButton-get-color
           GtkColorButton-get-title
           GtkColorButton-get-use-alpha?
           GtkColorButton-new
           GtkColorButton-new-with-color
           GtkColorButton-new-with-rgba
           GtkColorButton-set-alpha
           GtkColorButton-set-color
           GtkColorButton-set-title
           GtkColorButton-set-use-alpha
           GtkColorChooserDialog-new
           GtkColorChooserWidget-new
           GtkColorSelection-get-current-alpha
           GtkColorSelection-get-current-color
           GtkColorSelection-get-current-rgba
           GtkColorSelection-get-has-opacity-control?
           GtkColorSelection-get-has-palette?
           GtkColorSelection-get-previous-alpha
           GtkColorSelection-get-previous-color
           GtkColorSelection-get-previous-rgba
           GtkColorSelection-is-adjusting?
           GtkColorSelection-new
           GtkColorSelection-palette-from-string?
           GtkColorSelection-palette-to-string
           GtkColorSelection-set-current-alpha
           GtkColorSelection-set-current-color
           GtkColorSelection-set-current-rgba
           GtkColorSelection-set-has-opacity-control
           GtkColorSelection-set-has-palette
           GtkColorSelection-set-previous-alpha
           GtkColorSelection-set-previous-color
           GtkColorSelection-set-previous-rgba
           GtkColorSelectionDialog-get-color-selection
           GtkColorSelectionDialog-new
           GtkComboBox-get-active
           GtkComboBox-get-active-id
           GtkComboBox-get-active-iter?
           GtkComboBox-get-add-tearoffs?
           GtkComboBox-get-button-sensitivity
           GtkComboBox-get-column-span-column
           GtkComboBox-get-entry-text-column
           GtkComboBox-get-focus-on-click?
           GtkComboBox-get-has-entry?
           GtkComboBox-get-id-column
           GtkComboBox-get-model
           GtkComboBox-get-popup-accessible
           GtkComboBox-get-popup-fixed-width?
           GtkComboBox-get-row-span-column
           GtkComboBox-get-title
           GtkComboBox-get-wrap-width
           GtkComboBox-new
           GtkComboBox-new-with-area
           GtkComboBox-new-with-area-and-entry
           GtkComboBox-new-with-entry
           GtkComboBox-new-with-model
           GtkComboBox-new-with-model-and-entry
           GtkComboBox-popdown
           GtkComboBox-popup
           GtkComboBox-popup-for-device
           GtkComboBox-set-active
           GtkComboBox-set-active-id?
           GtkComboBox-set-active-iter
           GtkComboBox-set-add-tearoffs
           GtkComboBox-set-button-sensitivity
           GtkComboBox-set-column-span-column
           GtkComboBox-set-entry-text-column
           GtkComboBox-set-focus-on-click
           GtkComboBox-set-id-column
           GtkComboBox-set-model
           GtkComboBox-set-popup-fixed-width
           GtkComboBox-set-row-separator-func
           GtkComboBox-set-row-span-column
           GtkComboBox-set-title
           GtkComboBox-set-wrap-width
           GtkComboBoxText-append
           GtkComboBoxText-append-text
           GtkComboBoxText-get-active-text
           GtkComboBoxText-insert
           GtkComboBoxText-insert-text
           GtkComboBoxText-new
           GtkComboBoxText-new-with-entry
           GtkComboBoxText-prepend
           GtkComboBoxText-prepend-text
           GtkComboBoxText-remove
           GtkComboBoxText-remove-all
           GtkContainer-add
           GtkContainer-check-resize
           GtkContainer-child-get-property
           GtkContainer-child-notify
           GtkContainer-child-notify-by-pspec
           GtkContainer-child-set-property
           GtkContainer-child-type
           GtkContainer-forall
           GtkContainer-foreach
           GtkContainer-get-border-width
           GtkContainer-get-children
           GtkContainer-get-focus-chain?
           GtkContainer-get-focus-child
           GtkContainer-get-focus-hadjustment
           GtkContainer-get-focus-vadjustment
           GtkContainer-get-path-for-child
           GtkContainer-get-resize-mode
           GtkContainer-propagate-draw
           GtkContainer-remove
           GtkContainer-resize-children
           GtkContainer-set-border-width
           GtkContainer-set-focus-chain
           GtkContainer-set-focus-child
           GtkContainer-set-focus-hadjustment
           GtkContainer-set-focus-vadjustment
           GtkContainer-set-reallocate-redraws
           GtkContainer-set-resize-mode
           GtkContainer-unset-focus-chain
           GtkContainerCellAccessible-add-child
           GtkContainerCellAccessible-get-children
           GtkContainerCellAccessible-new
           GtkContainerCellAccessible-remove-child
           GtkCssProvider-get-default
           GtkCssProvider-get-named
           GtkCssProvider-load-from-data?
           GtkCssProvider-load-from-file?
           GtkCssProvider-load-from-path?
           GtkCssProvider-load-from-resource
           GtkCssProvider-new
           GtkCssProvider-to-string
           GtkCssSection-get-end-line
           GtkCssSection-get-end-position
           GtkCssSection-get-file
           GtkCssSection-get-parent
           GtkCssSection-get-section-type
           GtkCssSection-get-start-line
           GtkCssSection-get-start-position
           GtkCssSection-ref
           GtkCssSection-unref
           GtkDialog-add-action-widget
           GtkDialog-add-button
           GtkDialog-get-action-area
           GtkDialog-get-content-area
           GtkDialog-get-header-bar
           GtkDialog-get-response-for-widget
           GtkDialog-get-widget-for-response
           GtkDialog-new
           GtkDialog-response
           GtkDialog-run
           GtkDialog-set-alternative-button-order-from-array
           GtkDialog-set-default-response
           GtkDialog-set-response-sensitive
           GtkDrawingArea-new
           GtkEntry-get-activates-default?
           GtkEntry-get-alignment
           GtkEntry-get-attributes
           GtkEntry-get-buffer
           GtkEntry-get-completion
           GtkEntry-get-current-icon-drag-source
           GtkEntry-get-cursor-hadjustment
           GtkEntry-get-has-frame?
           GtkEntry-get-icon-activatable?
           GtkEntry-get-icon-area
           GtkEntry-get-icon-at-pos
           GtkEntry-get-icon-gicon
           GtkEntry-get-icon-name
           GtkEntry-get-icon-pixbuf
           GtkEntry-get-icon-sensitive?
           GtkEntry-get-icon-stock
           GtkEntry-get-icon-storage-type
           GtkEntry-get-icon-tooltip-markup
           GtkEntry-get-icon-tooltip-text
           GtkEntry-get-inner-border
           GtkEntry-get-input-hints
           GtkEntry-get-input-purpose
           GtkEntry-get-invisible-char
           GtkEntry-get-layout
           GtkEntry-get-layout-offsets
           GtkEntry-get-max-length
           GtkEntry-get-max-width-chars
           GtkEntry-get-overwrite-mode?
           GtkEntry-get-placeholder-text
           GtkEntry-get-progress-fraction
           GtkEntry-get-progress-pulse-step
           GtkEntry-get-tabs
           GtkEntry-get-text
           GtkEntry-get-text-area
           GtkEntry-get-text-length
           GtkEntry-get-visibility?
           GtkEntry-get-width-chars
           GtkEntry-grab-focus-without-selecting
           GtkEntry-im-context-filter-keypress?
           GtkEntry-layout-index-to-text-index
           GtkEntry-new
           GtkEntry-new-with-buffer
           GtkEntry-progress-pulse
           GtkEntry-reset-im-context
           GtkEntry-set-activates-default
           GtkEntry-set-alignment
           GtkEntry-set-attributes
           GtkEntry-set-buffer
           GtkEntry-set-completion
           GtkEntry-set-cursor-hadjustment
           GtkEntry-set-has-frame
           GtkEntry-set-icon-activatable
           GtkEntry-set-icon-drag-source
           GtkEntry-set-icon-from-gicon
           GtkEntry-set-icon-from-icon-name
           GtkEntry-set-icon-from-pixbuf
           GtkEntry-set-icon-from-stock
           GtkEntry-set-icon-sensitive
           GtkEntry-set-icon-tooltip-markup
           GtkEntry-set-icon-tooltip-text
           GtkEntry-set-inner-border
           GtkEntry-set-input-hints
           GtkEntry-set-input-purpose
           GtkEntry-set-invisible-char
           GtkEntry-set-max-length
           GtkEntry-set-max-width-chars
           GtkEntry-set-overwrite-mode
           GtkEntry-set-placeholder-text
           GtkEntry-set-progress-fraction
           GtkEntry-set-progress-pulse-step
           GtkEntry-set-tabs
           GtkEntry-set-text
           GtkEntry-set-visibility
           GtkEntry-set-width-chars
           GtkEntry-text-index-to-layout-index
           GtkEntry-unset-invisible-char
           GtkEntryBuffer-delete-text
           GtkEntryBuffer-emit-deleted-text
           GtkEntryBuffer-emit-inserted-text
           GtkEntryBuffer-get-bytes
           GtkEntryBuffer-get-length
           GtkEntryBuffer-get-max-length
           GtkEntryBuffer-get-text
           GtkEntryBuffer-insert-text
           GtkEntryBuffer-new
           GtkEntryBuffer-set-max-length
           GtkEntryBuffer-set-text
           GtkEntryCompletion-complete
           GtkEntryCompletion-compute-prefix
           GtkEntryCompletion-delete-action
           GtkEntryCompletion-get-completion-prefix
           GtkEntryCompletion-get-entry
           GtkEntryCompletion-get-inline-completion?
           GtkEntryCompletion-get-inline-selection?
           GtkEntryCompletion-get-minimum-key-length
           GtkEntryCompletion-get-model
           GtkEntryCompletion-get-popup-completion?
           GtkEntryCompletion-get-popup-set-width?
           GtkEntryCompletion-get-popup-single-match?
           GtkEntryCompletion-get-text-column
           GtkEntryCompletion-insert-action-markup
           GtkEntryCompletion-insert-action-text
           GtkEntryCompletion-insert-prefix
           GtkEntryCompletion-new
           GtkEntryCompletion-new-with-area
           GtkEntryCompletion-set-inline-completion
           GtkEntryCompletion-set-inline-selection
           GtkEntryCompletion-set-match-func
           GtkEntryCompletion-set-minimum-key-length
           GtkEntryCompletion-set-model
           GtkEntryCompletion-set-popup-completion
           GtkEntryCompletion-set-popup-set-width
           GtkEntryCompletion-set-popup-single-match
           GtkEntryCompletion-set-text-column
           GtkEventBox-get-above-child?
           GtkEventBox-get-visible-window?
           GtkEventBox-new
           GtkEventBox-set-above-child
           GtkEventBox-set-visible-window
           GtkEventController-get-propagation-phase
           GtkEventController-get-widget
           GtkEventController-handle-event?
           GtkEventController-reset
           GtkEventController-set-propagation-phase
           GtkEventControllerKey-forward?
           GtkEventControllerKey-get-group
           GtkEventControllerKey-get-im-context
           GtkEventControllerKey-new
           GtkEventControllerKey-set-im-context
           GtkEventControllerMotion-new
           GtkEventControllerScroll-get-flags
           GtkEventControllerScroll-new
           GtkEventControllerScroll-set-flags
           GtkExpander-get-expanded?
           GtkExpander-get-label
           GtkExpander-get-label-fill?
           GtkExpander-get-label-widget
           GtkExpander-get-resize-toplevel?
           GtkExpander-get-spacing
           GtkExpander-get-use-markup?
           GtkExpander-get-use-underline?
           GtkExpander-new
           GtkExpander-new-with-mnemonic
           GtkExpander-set-expanded
           GtkExpander-set-label
           GtkExpander-set-label-fill
           GtkExpander-set-label-widget
           GtkExpander-set-resize-toplevel
           GtkExpander-set-spacing
           GtkExpander-set-use-markup
           GtkExpander-set-use-underline
           GtkFileChooserButton-get-focus-on-click?
           GtkFileChooserButton-get-title
           GtkFileChooserButton-get-width-chars
           GtkFileChooserButton-new
           GtkFileChooserButton-new-with-dialog
           GtkFileChooserButton-set-focus-on-click
           GtkFileChooserButton-set-title
           GtkFileChooserButton-set-width-chars
           GtkFileChooserNative-get-accept-label
           GtkFileChooserNative-get-cancel-label
           GtkFileChooserNative-new
           GtkFileChooserNative-set-accept-label
           GtkFileChooserNative-set-cancel-label
           GtkFileChooserWidget-new
           GtkFileFilter-add-custom
           GtkFileFilter-add-mime-type
           GtkFileFilter-add-pattern
           GtkFileFilter-add-pixbuf-formats
           GtkFileFilter-filter?
           GtkFileFilter-get-name
           GtkFileFilter-get-needed
           GtkFileFilter-new
           GtkFileFilter-new-from-gvariant
           GtkFileFilter-set-name
           GtkFileFilter-to-gvariant
           GtkFixed-move
           GtkFixed-new
           GtkFixed-put
           GtkFlowBox-bind-model
           GtkFlowBox-get-activate-on-single-click?
           GtkFlowBox-get-child-at-index
           GtkFlowBox-get-child-at-pos
           GtkFlowBox-get-column-spacing
           GtkFlowBox-get-homogeneous?
           GtkFlowBox-get-max-children-per-line
           GtkFlowBox-get-min-children-per-line
           GtkFlowBox-get-row-spacing
           GtkFlowBox-get-selected-children
           GtkFlowBox-get-selection-mode
           GtkFlowBox-insert
           GtkFlowBox-invalidate-filter
           GtkFlowBox-invalidate-sort
           GtkFlowBox-new
           GtkFlowBox-select-all
           GtkFlowBox-select-child
           GtkFlowBox-selected-foreach
           GtkFlowBox-set-activate-on-single-click
           GtkFlowBox-set-column-spacing
           GtkFlowBox-set-filter-func
           GtkFlowBox-set-hadjustment
           GtkFlowBox-set-homogeneous
           GtkFlowBox-set-max-children-per-line
           GtkFlowBox-set-min-children-per-line
           GtkFlowBox-set-row-spacing
           GtkFlowBox-set-selection-mode
           GtkFlowBox-set-sort-func
           GtkFlowBox-set-vadjustment
           GtkFlowBox-unselect-all
           GtkFlowBox-unselect-child
           GtkFlowBoxChild-changed
           GtkFlowBoxChild-get-index
           GtkFlowBoxChild-is-selected?
           GtkFlowBoxChild-new
           GtkFontButton-get-font-name
           GtkFontButton-get-show-size?
           GtkFontButton-get-show-style?
           GtkFontButton-get-title
           GtkFontButton-get-use-font?
           GtkFontButton-get-use-size?
           GtkFontButton-new
           GtkFontButton-new-with-font
           GtkFontButton-set-font-name?
           GtkFontButton-set-show-size
           GtkFontButton-set-show-style
           GtkFontButton-set-title
           GtkFontButton-set-use-font
           GtkFontButton-set-use-size
           GtkFontChooserDialog-new
           GtkFontChooserWidget-new
           GtkFontSelection-get-face
           GtkFontSelection-get-face-list
           GtkFontSelection-get-family
           GtkFontSelection-get-family-list
           GtkFontSelection-get-font-name
           GtkFontSelection-get-preview-entry
           GtkFontSelection-get-preview-text
           GtkFontSelection-get-size
           GtkFontSelection-get-size-entry
           GtkFontSelection-get-size-list
           GtkFontSelection-new
           GtkFontSelection-set-font-name?
           GtkFontSelection-set-preview-text
           GtkFontSelectionDialog-get-cancel-button
           GtkFontSelectionDialog-get-font-name
           GtkFontSelectionDialog-get-font-selection
           GtkFontSelectionDialog-get-ok-button
           GtkFontSelectionDialog-get-preview-text
           GtkFontSelectionDialog-new
           GtkFontSelectionDialog-set-font-name?
           GtkFontSelectionDialog-set-preview-text
           GtkFrame-get-label
           GtkFrame-get-label-align
           GtkFrame-get-label-widget
           GtkFrame-get-shadow-type
           GtkFrame-new
           GtkFrame-set-label
           GtkFrame-set-label-align
           GtkFrame-set-label-widget
           GtkFrame-set-shadow-type
           GtkGLArea-attach-buffers
           GtkGLArea-get-auto-render?
           GtkGLArea-get-context
           GtkGLArea-get-error
           GtkGLArea-get-has-alpha?
           GtkGLArea-get-has-depth-buffer?
           GtkGLArea-get-has-stencil-buffer?
           GtkGLArea-get-required-version
           GtkGLArea-get-use-es?
           GtkGLArea-make-current
           GtkGLArea-new
           GtkGLArea-queue-render
           GtkGLArea-set-auto-render
           GtkGLArea-set-error
           GtkGLArea-set-has-alpha
           GtkGLArea-set-has-depth-buffer
           GtkGLArea-set-has-stencil-buffer
           GtkGLArea-set-required-version
           GtkGLArea-set-use-es
           GtkGesture-get-bounding-box-center?
           GtkGesture-get-bounding-box?
           GtkGesture-get-device
           GtkGesture-get-group
           GtkGesture-get-last-event
           GtkGesture-get-last-updated-sequence
           GtkGesture-get-point?
           GtkGesture-get-sequence-state
           GtkGesture-get-sequences
           GtkGesture-get-window
           GtkGesture-group
           GtkGesture-handles-sequence?
           GtkGesture-is-active?
           GtkGesture-is-grouped-with?
           GtkGesture-is-recognized?
           GtkGesture-set-sequence-state?
           GtkGesture-set-state?
           GtkGesture-set-window
           GtkGesture-ungroup
           GtkGestureDrag-get-offset?
           GtkGestureDrag-get-start-point?
           GtkGestureDrag-new
           GtkGestureLongPress-new
           GtkGestureMultiPress-get-area?
           GtkGestureMultiPress-new
           GtkGestureMultiPress-set-area
           GtkGesturePan-get-orientation
           GtkGesturePan-new
           GtkGesturePan-set-orientation
           GtkGestureRotate-get-angle-delta
           GtkGestureRotate-new
           GtkGestureSingle-get-button
           GtkGestureSingle-get-current-button
           GtkGestureSingle-get-current-sequence
           GtkGestureSingle-get-exclusive?
           GtkGestureSingle-get-touch-only?
           GtkGestureSingle-set-button
           GtkGestureSingle-set-exclusive
           GtkGestureSingle-set-touch-only
           GtkGestureStylus-get-axes?
           GtkGestureStylus-get-axis?
           GtkGestureStylus-get-device-tool
           GtkGestureStylus-new
           GtkGestureSwipe-get-velocity?
           GtkGestureSwipe-new
           GtkGestureZoom-get-scale-delta
           GtkGestureZoom-new
           GtkGradient-add-color-stop
           GtkGradient-new-linear
           GtkGradient-new-radial
           GtkGradient-ref
           GtkGradient-resolve-for-context
           GtkGradient-resolve?
           GtkGradient-to-string
           GtkGradient-unref
           GtkGrid-attach
           GtkGrid-attach-next-to
           GtkGrid-get-baseline-row
           GtkGrid-get-child-at
           GtkGrid-get-column-homogeneous?
           GtkGrid-get-column-spacing
           GtkGrid-get-row-baseline-position
           GtkGrid-get-row-homogeneous?
           GtkGrid-get-row-spacing
           GtkGrid-insert-column
           GtkGrid-insert-next-to
           GtkGrid-insert-row
           GtkGrid-new
           GtkGrid-remove-column
           GtkGrid-remove-row
           GtkGrid-set-baseline-row
           GtkGrid-set-column-homogeneous
           GtkGrid-set-column-spacing
           GtkGrid-set-row-baseline-position
           GtkGrid-set-row-homogeneous
           GtkGrid-set-row-spacing
           GtkHBox-new
           GtkHButtonBox-new
           GtkHPaned-new
           GtkHSV-get-color
           GtkHSV-get-metrics
           GtkHSV-is-adjusting?
           GtkHSV-new
           GtkHSV-set-color
           GtkHSV-set-metrics
           GtkHSV-to-rgb
           GtkHScale-new
           GtkHScale-new-with-range
           GtkHScrollbar-new
           GtkHSeparator-new
           GtkHandleBox-get-child-detached?
           GtkHandleBox-get-handle-position
           GtkHandleBox-get-shadow-type
           GtkHandleBox-get-snap-edge
           GtkHandleBox-new
           GtkHandleBox-set-handle-position
           GtkHandleBox-set-shadow-type
           GtkHandleBox-set-snap-edge
           GtkHeaderBar-get-custom-title
           GtkHeaderBar-get-decoration-layout
           GtkHeaderBar-get-has-subtitle?
           GtkHeaderBar-get-show-close-button?
           GtkHeaderBar-get-subtitle
           GtkHeaderBar-get-title
           GtkHeaderBar-new
           GtkHeaderBar-pack-end
           GtkHeaderBar-pack-start
           GtkHeaderBar-set-custom-title
           GtkHeaderBar-set-decoration-layout
           GtkHeaderBar-set-has-subtitle
           GtkHeaderBar-set-show-close-button
           GtkHeaderBar-set-subtitle
           GtkHeaderBar-set-title
           GtkIMContext-delete-surrounding?
           GtkIMContext-filter-keypress?
           GtkIMContext-focus-in
           GtkIMContext-focus-out
           GtkIMContext-get-preedit-string
           GtkIMContext-get-surrounding?
           GtkIMContext-reset
           GtkIMContext-set-client-window
           GtkIMContext-set-cursor-location
           GtkIMContext-set-surrounding
           GtkIMContext-set-use-preedit
           GtkIMContextSimple-add-compose-file
           GtkIMContextSimple-new
           GtkIMMulticontext-append-menuitems
           GtkIMMulticontext-get-context-id
           GtkIMMulticontext-new
           GtkIMMulticontext-set-context-id
           GtkIconFactory-add
           GtkIconFactory-add-default
           GtkIconFactory-lookup
           GtkIconFactory-lookup-default
           GtkIconFactory-new
           GtkIconFactory-remove-default
           GtkIconInfo-get-attach-points?
           GtkIconInfo-get-base-scale
           GtkIconInfo-get-base-size
           GtkIconInfo-get-builtin-pixbuf
           GtkIconInfo-get-display-name
           GtkIconInfo-get-embedded-rect?
           GtkIconInfo-get-filename
           GtkIconInfo-is-symbolic?
           GtkIconInfo-load-icon
           GtkIconInfo-load-icon-async
           GtkIconInfo-load-icon-finish
           GtkIconInfo-load-surface
           GtkIconInfo-load-symbolic
           GtkIconInfo-load-symbolic-async
           GtkIconInfo-load-symbolic-finish
           GtkIconInfo-load-symbolic-for-context
           GtkIconInfo-load-symbolic-for-context-async
           GtkIconInfo-load-symbolic-for-context-finish
           GtkIconInfo-load-symbolic-for-style
           GtkIconInfo-new-for-pixbuf
           GtkIconInfo-set-raw-coordinates
           GtkIconSet-add-source
           GtkIconSet-copy
           GtkIconSet-get-sizes
           GtkIconSet-new
           GtkIconSet-new-from-pixbuf
           GtkIconSet-ref
           GtkIconSet-render-icon
           GtkIconSet-render-icon-pixbuf
           GtkIconSet-render-icon-surface
           GtkIconSet-unref
           GtkIconSource-copy
           GtkIconSource-free
           GtkIconSource-get-direction
           GtkIconSource-get-direction-wildcarded?
           GtkIconSource-get-filename
           GtkIconSource-get-icon-name
           GtkIconSource-get-pixbuf
           GtkIconSource-get-size
           GtkIconSource-get-size-wildcarded?
           GtkIconSource-get-state
           GtkIconSource-get-state-wildcarded?
           GtkIconSource-new
           GtkIconSource-set-direction
           GtkIconSource-set-direction-wildcarded
           GtkIconSource-set-filename
           GtkIconSource-set-icon-name
           GtkIconSource-set-pixbuf
           GtkIconSource-set-size
           GtkIconSource-set-size-wildcarded
           GtkIconSource-set-state
           GtkIconSource-set-state-wildcarded
           GtkIconTheme-add-builtin-icon
           GtkIconTheme-add-resource-path
           GtkIconTheme-append-search-path
           GtkIconTheme-choose-icon
           GtkIconTheme-choose-icon-for-scale
           GtkIconTheme-get-default
           GtkIconTheme-get-example-icon-name
           GtkIconTheme-get-for-screen
           GtkIconTheme-get-icon-sizes
           GtkIconTheme-get-search-path
           GtkIconTheme-has-icon?
           GtkIconTheme-list-contexts
           GtkIconTheme-list-icons
           GtkIconTheme-load-icon
           GtkIconTheme-load-icon-for-scale
           GtkIconTheme-load-surface
           GtkIconTheme-lookup-by-gicon
           GtkIconTheme-lookup-by-gicon-for-scale
           GtkIconTheme-lookup-icon
           GtkIconTheme-lookup-icon-for-scale
           GtkIconTheme-new
           GtkIconTheme-prepend-search-path
           GtkIconTheme-rescan-if-needed?
           GtkIconTheme-set-custom-theme
           GtkIconTheme-set-screen
           GtkIconTheme-set-search-path
           GtkIconView-convert-widget-to-bin-window-coords
           GtkIconView-create-drag-icon
           GtkIconView-enable-model-drag-dest
           GtkIconView-enable-model-drag-source
           GtkIconView-get-activate-on-single-click?
           GtkIconView-get-cell-rect?
           GtkIconView-get-column-spacing
           GtkIconView-get-columns
           GtkIconView-get-cursor?
           GtkIconView-get-dest-item-at-pos?
           GtkIconView-get-drag-dest-item
           GtkIconView-get-item-at-pos?
           GtkIconView-get-item-column
           GtkIconView-get-item-orientation
           GtkIconView-get-item-padding
           GtkIconView-get-item-row
           GtkIconView-get-item-width
           GtkIconView-get-margin
           GtkIconView-get-markup-column
           GtkIconView-get-model
           GtkIconView-get-path-at-pos
           GtkIconView-get-pixbuf-column
           GtkIconView-get-reorderable?
           GtkIconView-get-row-spacing
           GtkIconView-get-selected-items
           GtkIconView-get-selection-mode
           GtkIconView-get-spacing
           GtkIconView-get-text-column
           GtkIconView-get-tooltip-column
           GtkIconView-get-tooltip-context?
           GtkIconView-get-visible-range?
           GtkIconView-item-activated
           GtkIconView-new
           GtkIconView-new-with-area
           GtkIconView-new-with-model
           GtkIconView-path-is-selected?
           GtkIconView-scroll-to-path
           GtkIconView-select-all
           GtkIconView-select-path
           GtkIconView-selected-foreach
           GtkIconView-set-activate-on-single-click
           GtkIconView-set-column-spacing
           GtkIconView-set-columns
           GtkIconView-set-cursor
           GtkIconView-set-drag-dest-item
           GtkIconView-set-item-orientation
           GtkIconView-set-item-padding
           GtkIconView-set-item-width
           GtkIconView-set-margin
           GtkIconView-set-markup-column
           GtkIconView-set-model
           GtkIconView-set-pixbuf-column
           GtkIconView-set-reorderable
           GtkIconView-set-row-spacing
           GtkIconView-set-selection-mode
           GtkIconView-set-spacing
           GtkIconView-set-text-column
           GtkIconView-set-tooltip-cell
           GtkIconView-set-tooltip-column
           GtkIconView-set-tooltip-item
           GtkIconView-unselect-all
           GtkIconView-unselect-path
           GtkIconView-unset-model-drag-dest
           GtkIconView-unset-model-drag-source
           GtkImage-clear
           GtkImage-get-animation
           GtkImage-get-gicon
           GtkImage-get-icon-name
           GtkImage-get-icon-set
           GtkImage-get-pixbuf
           GtkImage-get-pixel-size
           GtkImage-get-stock
           GtkImage-get-storage-type
           GtkImage-new
           GtkImage-new-from-animation
           GtkImage-new-from-file
           GtkImage-new-from-gicon
           GtkImage-new-from-icon-name
           GtkImage-new-from-icon-set
           GtkImage-new-from-pixbuf
           GtkImage-new-from-resource
           GtkImage-new-from-stock
           GtkImage-new-from-surface
           GtkImage-set-from-animation
           GtkImage-set-from-file
           GtkImage-set-from-gicon
           GtkImage-set-from-icon-name
           GtkImage-set-from-icon-set
           GtkImage-set-from-pixbuf
           GtkImage-set-from-resource
           GtkImage-set-from-stock
           GtkImage-set-from-surface
           GtkImage-set-pixel-size
           GtkImageMenuItem-get-always-show-image?
           GtkImageMenuItem-get-image
           GtkImageMenuItem-get-use-stock?
           GtkImageMenuItem-new
           GtkImageMenuItem-new-from-stock
           GtkImageMenuItem-new-with-label
           GtkImageMenuItem-new-with-mnemonic
           GtkImageMenuItem-set-accel-group
           GtkImageMenuItem-set-always-show-image
           GtkImageMenuItem-set-image
           GtkImageMenuItem-set-use-stock
           GtkInfoBar-add-action-widget
           GtkInfoBar-add-button
           GtkInfoBar-get-action-area
           GtkInfoBar-get-content-area
           GtkInfoBar-get-message-type
           GtkInfoBar-get-revealed?
           GtkInfoBar-get-show-close-button?
           GtkInfoBar-new
           GtkInfoBar-response
           GtkInfoBar-set-default-response
           GtkInfoBar-set-message-type
           GtkInfoBar-set-response-sensitive
           GtkInfoBar-set-revealed
           GtkInfoBar-set-show-close-button
           GtkInvisible-get-screen
           GtkInvisible-new
           GtkInvisible-new-for-screen
           GtkInvisible-set-screen
           GtkLabel-get-angle
           GtkLabel-get-attributes
           GtkLabel-get-current-uri
           GtkLabel-get-ellipsize
           GtkLabel-get-justify
           GtkLabel-get-label
           GtkLabel-get-layout
           GtkLabel-get-layout-offsets
           GtkLabel-get-line-wrap-mode
           GtkLabel-get-line-wrap?
           GtkLabel-get-lines
           GtkLabel-get-max-width-chars
           GtkLabel-get-mnemonic-keyval
           GtkLabel-get-mnemonic-widget
           GtkLabel-get-selectable?
           GtkLabel-get-selection-bounds?
           GtkLabel-get-single-line-mode?
           GtkLabel-get-text
           GtkLabel-get-track-visited-links?
           GtkLabel-get-use-markup?
           GtkLabel-get-use-underline?
           GtkLabel-get-width-chars
           GtkLabel-get-xalign
           GtkLabel-get-yalign
           GtkLabel-new
           GtkLabel-new-with-mnemonic
           GtkLabel-select-region
           GtkLabel-set-angle
           GtkLabel-set-attributes
           GtkLabel-set-ellipsize
           GtkLabel-set-justify
           GtkLabel-set-label
           GtkLabel-set-line-wrap
           GtkLabel-set-line-wrap-mode
           GtkLabel-set-lines
           GtkLabel-set-markup
           GtkLabel-set-markup-with-mnemonic
           GtkLabel-set-max-width-chars
           GtkLabel-set-mnemonic-widget
           GtkLabel-set-pattern
           GtkLabel-set-selectable
           GtkLabel-set-single-line-mode
           GtkLabel-set-text
           GtkLabel-set-text-with-mnemonic
           GtkLabel-set-track-visited-links
           GtkLabel-set-use-markup
           GtkLabel-set-use-underline
           GtkLabel-set-width-chars
           GtkLabel-set-xalign
           GtkLabel-set-yalign
           GtkLayout-get-bin-window
           GtkLayout-get-hadjustment
           GtkLayout-get-size
           GtkLayout-get-vadjustment
           GtkLayout-move
           GtkLayout-new
           GtkLayout-put
           GtkLayout-set-hadjustment
           GtkLayout-set-size
           GtkLayout-set-vadjustment
           GtkLevelBar-add-offset-value
           GtkLevelBar-get-inverted?
           GtkLevelBar-get-max-value
           GtkLevelBar-get-min-value
           GtkLevelBar-get-mode
           GtkLevelBar-get-offset-value?
           GtkLevelBar-get-value
           GtkLevelBar-new
           GtkLevelBar-new-for-interval
           GtkLevelBar-remove-offset-value
           GtkLevelBar-set-inverted
           GtkLevelBar-set-max-value
           GtkLevelBar-set-min-value
           GtkLevelBar-set-mode
           GtkLevelBar-set-value
           GtkLinkButton-get-uri
           GtkLinkButton-get-visited?
           GtkLinkButton-new
           GtkLinkButton-new-with-label
           GtkLinkButton-set-uri
           GtkLinkButton-set-visited
           GtkListBox-bind-model
           GtkListBox-drag-highlight-row
           GtkListBox-drag-unhighlight-row
           GtkListBox-get-activate-on-single-click?
           GtkListBox-get-adjustment
           GtkListBox-get-row-at-index
           GtkListBox-get-row-at-y
           GtkListBox-get-selected-row
           GtkListBox-get-selected-rows
           GtkListBox-get-selection-mode
           GtkListBox-insert
           GtkListBox-invalidate-filter
           GtkListBox-invalidate-headers
           GtkListBox-invalidate-sort
           GtkListBox-new
           GtkListBox-prepend
           GtkListBox-select-all
           GtkListBox-select-row
           GtkListBox-selected-foreach
           GtkListBox-set-activate-on-single-click
           GtkListBox-set-adjustment
           GtkListBox-set-filter-func
           GtkListBox-set-header-func
           GtkListBox-set-placeholder
           GtkListBox-set-selection-mode
           GtkListBox-set-sort-func
           GtkListBox-unselect-all
           GtkListBox-unselect-row
           GtkListBoxRow-changed
           GtkListBoxRow-get-activatable?
           GtkListBoxRow-get-header
           GtkListBoxRow-get-index
           GtkListBoxRow-get-selectable?
           GtkListBoxRow-is-selected?
           GtkListBoxRow-new
           GtkListBoxRow-set-activatable
           GtkListBoxRow-set-header
           GtkListBoxRow-set-selectable
           GtkListStore-append
           GtkListStore-clear
           GtkListStore-insert
           GtkListStore-insert-after
           GtkListStore-insert-before
           GtkListStore-insert-with-valuesv
           GtkListStore-iter-is-valid?
           GtkListStore-move-after
           GtkListStore-move-before
           GtkListStore-new
           GtkListStore-prepend
           GtkListStore-remove?
           GtkListStore-reorder
           GtkListStore-set
           GtkListStore-set-column-types
           GtkListStore-set-value
           GtkListStore-swap
           GtkLockButton-get-permission
           GtkLockButton-new
           GtkLockButton-set-permission
           GtkMenu-attach
           GtkMenu-attach-to-widget
           GtkMenu-detach
           GtkMenu-get-accel-group
           GtkMenu-get-accel-path
           GtkMenu-get-active
           GtkMenu-get-attach-widget
           GtkMenu-get-for-attach-widget
           GtkMenu-get-monitor
           GtkMenu-get-reserve-toggle-size?
           GtkMenu-get-tearoff-state?
           GtkMenu-get-title
           GtkMenu-new
           GtkMenu-new-from-model
           GtkMenu-place-on-monitor
           GtkMenu-popdown
           GtkMenu-popup
           GtkMenu-popup-at-pointer
           GtkMenu-popup-at-rect
           GtkMenu-popup-at-widget
           GtkMenu-popup-for-device
           GtkMenu-reorder-child
           GtkMenu-reposition
           GtkMenu-set-accel-group
           GtkMenu-set-accel-path
           GtkMenu-set-active
           GtkMenu-set-monitor
           GtkMenu-set-reserve-toggle-size
           GtkMenu-set-screen
           GtkMenu-set-tearoff-state
           GtkMenu-set-title
           GtkMenuBar-get-child-pack-direction
           GtkMenuBar-get-pack-direction
           GtkMenuBar-new
           GtkMenuBar-new-from-model
           GtkMenuBar-set-child-pack-direction
           GtkMenuBar-set-pack-direction
           GtkMenuButton-get-align-widget
           GtkMenuButton-get-direction
           GtkMenuButton-get-menu-model
           GtkMenuButton-get-popover
           GtkMenuButton-get-popup
           GtkMenuButton-get-use-popover?
           GtkMenuButton-new
           GtkMenuButton-set-align-widget
           GtkMenuButton-set-direction
           GtkMenuButton-set-menu-model
           GtkMenuButton-set-popover
           GtkMenuButton-set-popup
           GtkMenuButton-set-use-popover
           GtkMenuItem-activate
           GtkMenuItem-deselect
           GtkMenuItem-get-accel-path
           GtkMenuItem-get-label
           GtkMenuItem-get-reserve-indicator?
           GtkMenuItem-get-right-justified?
           GtkMenuItem-get-submenu
           GtkMenuItem-get-use-underline?
           GtkMenuItem-new
           GtkMenuItem-new-with-label
           GtkMenuItem-new-with-mnemonic
           GtkMenuItem-select
           GtkMenuItem-set-accel-path
           GtkMenuItem-set-label
           GtkMenuItem-set-reserve-indicator
           GtkMenuItem-set-right-justified
           GtkMenuItem-set-submenu
           GtkMenuItem-set-use-underline
           GtkMenuItem-toggle-size-allocate
           GtkMenuItem-toggle-size-request
           GtkMenuShell-activate-item
           GtkMenuShell-append
           GtkMenuShell-bind-model
           GtkMenuShell-cancel
           GtkMenuShell-deactivate
           GtkMenuShell-deselect
           GtkMenuShell-get-parent-shell
           GtkMenuShell-get-selected-item
           GtkMenuShell-get-take-focus?
           GtkMenuShell-insert
           GtkMenuShell-prepend
           GtkMenuShell-select-first
           GtkMenuShell-select-item
           GtkMenuShell-set-take-focus
           GtkMenuToolButton-get-menu
           GtkMenuToolButton-new
           GtkMenuToolButton-new-from-stock
           GtkMenuToolButton-set-arrow-tooltip-markup
           GtkMenuToolButton-set-arrow-tooltip-text
           GtkMenuToolButton-set-menu
           GtkMessageDialog-get-image
           GtkMessageDialog-get-message-area
           GtkMessageDialog-set-image
           GtkMessageDialog-set-markup
           GtkMisc-get-alignment
           GtkMisc-get-padding
           GtkMisc-set-alignment
           GtkMisc-set-padding
           GtkModelButton-new
           GtkMountOperation-get-parent
           GtkMountOperation-get-screen
           GtkMountOperation-is-showing?
           GtkMountOperation-new
           GtkMountOperation-set-parent
           GtkMountOperation-set-screen
           GtkNativeDialog-destroy
           GtkNativeDialog-get-modal?
           GtkNativeDialog-get-title
           GtkNativeDialog-get-transient-for
           GtkNativeDialog-get-visible?
           GtkNativeDialog-hide
           GtkNativeDialog-run
           GtkNativeDialog-set-modal
           GtkNativeDialog-set-title
           GtkNativeDialog-set-transient-for
           GtkNativeDialog-show
           GtkNotebook-append-page
           GtkNotebook-append-page-menu
           GtkNotebook-detach-tab
           GtkNotebook-get-action-widget
           GtkNotebook-get-current-page
           GtkNotebook-get-group-name
           GtkNotebook-get-menu-label
           GtkNotebook-get-menu-label-text
           GtkNotebook-get-n-pages
           GtkNotebook-get-nth-page
           GtkNotebook-get-scrollable?
           GtkNotebook-get-show-border?
           GtkNotebook-get-show-tabs?
           GtkNotebook-get-tab-detachable?
           GtkNotebook-get-tab-hborder
           GtkNotebook-get-tab-label
           GtkNotebook-get-tab-label-text
           GtkNotebook-get-tab-pos
           GtkNotebook-get-tab-reorderable?
           GtkNotebook-get-tab-vborder
           GtkNotebook-insert-page
           GtkNotebook-insert-page-menu
           GtkNotebook-new
           GtkNotebook-next-page
           GtkNotebook-page-num
           GtkNotebook-popup-disable
           GtkNotebook-popup-enable
           GtkNotebook-prepend-page
           GtkNotebook-prepend-page-menu
           GtkNotebook-prev-page
           GtkNotebook-remove-page
           GtkNotebook-reorder-child
           GtkNotebook-set-action-widget
           GtkNotebook-set-current-page
           GtkNotebook-set-group-name
           GtkNotebook-set-menu-label
           GtkNotebook-set-menu-label-text
           GtkNotebook-set-scrollable
           GtkNotebook-set-show-border
           GtkNotebook-set-show-tabs
           GtkNotebook-set-tab-detachable
           GtkNotebook-set-tab-label
           GtkNotebook-set-tab-label-text
           GtkNotebook-set-tab-pos
           GtkNotebook-set-tab-reorderable
           GtkNotebookPageAccessible-invalidate
           GtkNotebookPageAccessible-new
           GtkNumerableIcon-get-background-gicon
           GtkNumerableIcon-get-background-icon-name
           GtkNumerableIcon-get-count
           GtkNumerableIcon-get-label
           GtkNumerableIcon-get-style-context
           GtkNumerableIcon-new
           GtkNumerableIcon-new-with-style-context
           GtkNumerableIcon-set-background-gicon
           GtkNumerableIcon-set-background-icon-name
           GtkNumerableIcon-set-count
           GtkNumerableIcon-set-label
           GtkNumerableIcon-set-style-context
           GtkOffscreenWindow-get-pixbuf
           GtkOffscreenWindow-get-surface
           GtkOffscreenWindow-new
           GtkOverlay-add-overlay
           GtkOverlay-get-overlay-pass-through?
           GtkOverlay-new
           GtkOverlay-reorder-overlay
           GtkOverlay-set-overlay-pass-through
           GtkPadController-new
           GtkPadController-set-action
           GtkPadController-set-action-entries
           GtkPageSetup-copy
           GtkPageSetup-get-bottom-margin
           GtkPageSetup-get-left-margin
           GtkPageSetup-get-orientation
           GtkPageSetup-get-page-height
           GtkPageSetup-get-page-width
           GtkPageSetup-get-paper-height
           GtkPageSetup-get-paper-size
           GtkPageSetup-get-paper-width
           GtkPageSetup-get-right-margin
           GtkPageSetup-get-top-margin
           GtkPageSetup-load-file?
           GtkPageSetup-load-key-file?
           GtkPageSetup-new
           GtkPageSetup-new-from-file
           GtkPageSetup-new-from-gvariant
           GtkPageSetup-new-from-key-file
           GtkPageSetup-set-bottom-margin
           GtkPageSetup-set-left-margin
           GtkPageSetup-set-orientation
           GtkPageSetup-set-paper-size
           GtkPageSetup-set-paper-size-and-default-margins
           GtkPageSetup-set-right-margin
           GtkPageSetup-set-top-margin
           GtkPageSetup-to-file?
           GtkPageSetup-to-gvariant
           GtkPageSetup-to-key-file
           GtkPaned-add1
           GtkPaned-add2
           GtkPaned-get-child1
           GtkPaned-get-child2
           GtkPaned-get-handle-window
           GtkPaned-get-position
           GtkPaned-get-wide-handle?
           GtkPaned-new
           GtkPaned-pack1
           GtkPaned-pack2
           GtkPaned-set-position
           GtkPaned-set-wide-handle
           GtkPaperSize-copy
           GtkPaperSize-free
           GtkPaperSize-get-default
           GtkPaperSize-get-default-bottom-margin
           GtkPaperSize-get-default-left-margin
           GtkPaperSize-get-default-right-margin
           GtkPaperSize-get-default-top-margin
           GtkPaperSize-get-display-name
           GtkPaperSize-get-height
           GtkPaperSize-get-name
           GtkPaperSize-get-paper-sizes
           GtkPaperSize-get-ppd-name
           GtkPaperSize-get-width
           GtkPaperSize-is-custom?
           GtkPaperSize-is-equal?
           GtkPaperSize-is-ipp?
           GtkPaperSize-new
           GtkPaperSize-new-custom
           GtkPaperSize-new-from-gvariant
           GtkPaperSize-new-from-ipp
           GtkPaperSize-new-from-key-file
           GtkPaperSize-new-from-ppd
           GtkPaperSize-set-size
           GtkPaperSize-to-gvariant
           GtkPaperSize-to-key-file
           GtkPlacesSidebar-add-shortcut
           GtkPlacesSidebar-get-local-only?
           GtkPlacesSidebar-get-location
           GtkPlacesSidebar-get-nth-bookmark
           GtkPlacesSidebar-get-open-flags
           GtkPlacesSidebar-get-show-connect-to-server?
           GtkPlacesSidebar-get-show-desktop?
           GtkPlacesSidebar-get-show-enter-location?
           GtkPlacesSidebar-get-show-other-locations?
           GtkPlacesSidebar-get-show-recent?
           GtkPlacesSidebar-get-show-starred-location?
           GtkPlacesSidebar-get-show-trash?
           GtkPlacesSidebar-list-shortcuts
           GtkPlacesSidebar-new
           GtkPlacesSidebar-remove-shortcut
           GtkPlacesSidebar-set-drop-targets-visible
           GtkPlacesSidebar-set-local-only
           GtkPlacesSidebar-set-location
           GtkPlacesSidebar-set-open-flags
           GtkPlacesSidebar-set-show-connect-to-server
           GtkPlacesSidebar-set-show-desktop
           GtkPlacesSidebar-set-show-enter-location
           GtkPlacesSidebar-set-show-other-locations
           GtkPlacesSidebar-set-show-recent
           GtkPlacesSidebar-set-show-starred-location
           GtkPlacesSidebar-set-show-trash
           GtkPopover-bind-model
           GtkPopover-get-constrain-to
           GtkPopover-get-default-widget
           GtkPopover-get-modal?
           GtkPopover-get-pointing-to?
           GtkPopover-get-position
           GtkPopover-get-relative-to
           GtkPopover-get-transitions-enabled?
           GtkPopover-new
           GtkPopover-new-from-model
           GtkPopover-popdown
           GtkPopover-popup
           GtkPopover-set-constrain-to
           GtkPopover-set-default-widget
           GtkPopover-set-modal
           GtkPopover-set-pointing-to
           GtkPopover-set-position
           GtkPopover-set-relative-to
           GtkPopover-set-transitions-enabled
           GtkPopoverMenu-new
           GtkPopoverMenu-open-submenu
           GtkPrintContext-create-pango-context
           GtkPrintContext-create-pango-layout
           GtkPrintContext-get-cairo-context
           GtkPrintContext-get-dpi-x
           GtkPrintContext-get-dpi-y
           GtkPrintContext-get-hard-margins?
           GtkPrintContext-get-height
           GtkPrintContext-get-page-setup
           GtkPrintContext-get-pango-fontmap
           GtkPrintContext-get-width
           GtkPrintContext-set-cairo-context
           GtkPrintOperation-cancel
           GtkPrintOperation-draw-page-finish
           GtkPrintOperation-get-default-page-setup
           GtkPrintOperation-get-embed-page-setup?
           GtkPrintOperation-get-error
           GtkPrintOperation-get-has-selection?
           GtkPrintOperation-get-n-pages-to-print
           GtkPrintOperation-get-print-settings
           GtkPrintOperation-get-status
           GtkPrintOperation-get-status-string
           GtkPrintOperation-get-support-selection?
           GtkPrintOperation-is-finished?
           GtkPrintOperation-new
           GtkPrintOperation-run
           GtkPrintOperation-set-allow-async
           GtkPrintOperation-set-current-page
           GtkPrintOperation-set-custom-tab-label
           GtkPrintOperation-set-default-page-setup
           GtkPrintOperation-set-defer-drawing
           GtkPrintOperation-set-embed-page-setup
           GtkPrintOperation-set-export-filename
           GtkPrintOperation-set-has-selection
           GtkPrintOperation-set-job-name
           GtkPrintOperation-set-n-pages
           GtkPrintOperation-set-print-settings
           GtkPrintOperation-set-show-progress
           GtkPrintOperation-set-support-selection
           GtkPrintOperation-set-track-print-status
           GtkPrintOperation-set-unit
           GtkPrintOperation-set-use-full-page
           GtkPrintSettings-copy
           GtkPrintSettings-foreach
           GtkPrintSettings-get
           GtkPrintSettings-get-bool?
           GtkPrintSettings-get-collate?
           GtkPrintSettings-get-default-source
           GtkPrintSettings-get-dither
           GtkPrintSettings-get-double
           GtkPrintSettings-get-double-with-default
           GtkPrintSettings-get-duplex
           GtkPrintSettings-get-finishings
           GtkPrintSettings-get-int
           GtkPrintSettings-get-int-with-default
           GtkPrintSettings-get-length
           GtkPrintSettings-get-media-type
           GtkPrintSettings-get-n-copies
           GtkPrintSettings-get-number-up
           GtkPrintSettings-get-number-up-layout
           GtkPrintSettings-get-orientation
           GtkPrintSettings-get-output-bin
           GtkPrintSettings-get-page-ranges
           GtkPrintSettings-get-page-set
           GtkPrintSettings-get-paper-height
           GtkPrintSettings-get-paper-size
           GtkPrintSettings-get-paper-width
           GtkPrintSettings-get-print-pages
           GtkPrintSettings-get-printer
           GtkPrintSettings-get-printer-lpi
           GtkPrintSettings-get-quality
           GtkPrintSettings-get-resolution
           GtkPrintSettings-get-resolution-x
           GtkPrintSettings-get-resolution-y
           GtkPrintSettings-get-reverse?
           GtkPrintSettings-get-scale
           GtkPrintSettings-get-use-color?
           GtkPrintSettings-has-key?
           GtkPrintSettings-load-file?
           GtkPrintSettings-load-key-file?
           GtkPrintSettings-new
           GtkPrintSettings-new-from-file
           GtkPrintSettings-new-from-gvariant
           GtkPrintSettings-new-from-key-file
           GtkPrintSettings-set
           GtkPrintSettings-set-bool
           GtkPrintSettings-set-collate
           GtkPrintSettings-set-default-source
           GtkPrintSettings-set-dither
           GtkPrintSettings-set-double
           GtkPrintSettings-set-duplex
           GtkPrintSettings-set-finishings
           GtkPrintSettings-set-int
           GtkPrintSettings-set-length
           GtkPrintSettings-set-media-type
           GtkPrintSettings-set-n-copies
           GtkPrintSettings-set-number-up
           GtkPrintSettings-set-number-up-layout
           GtkPrintSettings-set-orientation
           GtkPrintSettings-set-output-bin
           GtkPrintSettings-set-page-ranges
           GtkPrintSettings-set-page-set
           GtkPrintSettings-set-paper-height
           GtkPrintSettings-set-paper-size
           GtkPrintSettings-set-paper-width
           GtkPrintSettings-set-print-pages
           GtkPrintSettings-set-printer
           GtkPrintSettings-set-printer-lpi
           GtkPrintSettings-set-quality
           GtkPrintSettings-set-resolution
           GtkPrintSettings-set-resolution-xy
           GtkPrintSettings-set-reverse
           GtkPrintSettings-set-scale
           GtkPrintSettings-set-use-color
           GtkPrintSettings-to-file?
           GtkPrintSettings-to-gvariant
           GtkPrintSettings-to-key-file
           GtkPrintSettings-unset
           GtkProgressBar-get-ellipsize
           GtkProgressBar-get-fraction
           GtkProgressBar-get-inverted?
           GtkProgressBar-get-pulse-step
           GtkProgressBar-get-show-text?
           GtkProgressBar-get-text
           GtkProgressBar-new
           GtkProgressBar-pulse
           GtkProgressBar-set-ellipsize
           GtkProgressBar-set-fraction
           GtkProgressBar-set-inverted
           GtkProgressBar-set-pulse-step
           GtkProgressBar-set-show-text
           GtkProgressBar-set-text
           GtkRadioAction-get-current-value
           GtkRadioAction-get-group
           GtkRadioAction-join-group
           GtkRadioAction-new
           GtkRadioAction-set-current-value
           GtkRadioAction-set-group
           GtkRadioButton-get-group
           GtkRadioButton-join-group
           GtkRadioButton-new
           GtkRadioButton-new-from-widget
           GtkRadioButton-new-with-label
           GtkRadioButton-new-with-label-from-widget
           GtkRadioButton-new-with-mnemonic
           GtkRadioButton-new-with-mnemonic-from-widget
           GtkRadioButton-set-group
           GtkRadioMenuItem-get-group
           GtkRadioMenuItem-join-group
           GtkRadioMenuItem-new
           GtkRadioMenuItem-new-from-widget
           GtkRadioMenuItem-new-with-label
           GtkRadioMenuItem-new-with-label-from-widget
           GtkRadioMenuItem-new-with-mnemonic
           GtkRadioMenuItem-new-with-mnemonic-from-widget
           GtkRadioMenuItem-set-group
           GtkRadioToolButton-get-group
           GtkRadioToolButton-new
           GtkRadioToolButton-new-from-stock
           GtkRadioToolButton-new-from-widget
           GtkRadioToolButton-new-with-stock-from-widget
           GtkRadioToolButton-set-group
           GtkRange-get-adjustment
           GtkRange-get-fill-level
           GtkRange-get-flippable?
           GtkRange-get-inverted?
           GtkRange-get-lower-stepper-sensitivity
           GtkRange-get-min-slider-size
           GtkRange-get-range-rect
           GtkRange-get-restrict-to-fill-level?
           GtkRange-get-round-digits
           GtkRange-get-show-fill-level?
           GtkRange-get-slider-range
           GtkRange-get-slider-size-fixed?
           GtkRange-get-upper-stepper-sensitivity
           GtkRange-get-value
           GtkRange-set-adjustment
           GtkRange-set-fill-level
           GtkRange-set-flippable
           GtkRange-set-increments
           GtkRange-set-inverted
           GtkRange-set-lower-stepper-sensitivity
           GtkRange-set-min-slider-size
           GtkRange-set-range
           GtkRange-set-restrict-to-fill-level
           GtkRange-set-round-digits
           GtkRange-set-show-fill-level
           GtkRange-set-slider-size-fixed
           GtkRange-set-upper-stepper-sensitivity
           GtkRange-set-value
           GtkRcStyle-copy
           GtkRcStyle-new
           GtkRecentAction-get-show-numbers?
           GtkRecentAction-new
           GtkRecentAction-new-for-manager
           GtkRecentAction-set-show-numbers
           GtkRecentChooserMenu-get-show-numbers?
           GtkRecentChooserMenu-new
           GtkRecentChooserMenu-new-for-manager
           GtkRecentChooserMenu-set-show-numbers
           GtkRecentChooserWidget-new
           GtkRecentChooserWidget-new-for-manager
           GtkRecentFilter-add-age
           GtkRecentFilter-add-application
           GtkRecentFilter-add-custom
           GtkRecentFilter-add-group
           GtkRecentFilter-add-mime-type
           GtkRecentFilter-add-pattern
           GtkRecentFilter-add-pixbuf-formats
           GtkRecentFilter-filter?
           GtkRecentFilter-get-name
           GtkRecentFilter-get-needed
           GtkRecentFilter-new
           GtkRecentFilter-set-name
           GtkRecentInfo-create-app-info
           GtkRecentInfo-exists?
           GtkRecentInfo-get-added
           GtkRecentInfo-get-age
           GtkRecentInfo-get-application-info?
           GtkRecentInfo-get-applications
           GtkRecentInfo-get-description
           GtkRecentInfo-get-display-name
           GtkRecentInfo-get-gicon
           GtkRecentInfo-get-groups
           GtkRecentInfo-get-icon
           GtkRecentInfo-get-mime-type
           GtkRecentInfo-get-modified
           GtkRecentInfo-get-private-hint?
           GtkRecentInfo-get-short-name
           GtkRecentInfo-get-uri
           GtkRecentInfo-get-uri-display
           GtkRecentInfo-get-visited
           GtkRecentInfo-has-application?
           GtkRecentInfo-has-group?
           GtkRecentInfo-is-local?
           GtkRecentInfo-last-application
           GtkRecentInfo-match?
           GtkRecentInfo-ref
           GtkRecentInfo-unref
           GtkRecentManager-add-full?
           GtkRecentManager-add-item?
           GtkRecentManager-get-default
           GtkRecentManager-get-items
           GtkRecentManager-has-item?
           GtkRecentManager-lookup-item
           GtkRecentManager-move-item?
           GtkRecentManager-new
           GtkRecentManager-purge-items
           GtkRecentManager-remove-item?
           GtkRendererCellAccessible-new
           GtkRequisition-copy
           GtkRequisition-free
           GtkRequisition-new
           GtkRevealer-get-child-revealed?
           GtkRevealer-get-reveal-child?
           GtkRevealer-get-transition-duration
           GtkRevealer-get-transition-type
           GtkRevealer-new
           GtkRevealer-set-reveal-child
           GtkRevealer-set-transition-duration
           GtkRevealer-set-transition-type
           GtkScale-add-mark
           GtkScale-clear-marks
           GtkScale-get-digits
           GtkScale-get-draw-value?
           GtkScale-get-has-origin?
           GtkScale-get-layout
           GtkScale-get-layout-offsets
           GtkScale-get-value-pos
           GtkScale-new
           GtkScale-new-with-range
           GtkScale-set-digits
           GtkScale-set-draw-value
           GtkScale-set-has-origin
           GtkScale-set-value-pos
           GtkScaleButton-get-adjustment
           GtkScaleButton-get-minus-button
           GtkScaleButton-get-plus-button
           GtkScaleButton-get-popup
           GtkScaleButton-get-value
           GtkScaleButton-new
           GtkScaleButton-set-adjustment
           GtkScaleButton-set-icons
           GtkScaleButton-set-value
           GtkScrollbar-new
           GtkScrolledWindow-add-with-viewport
           GtkScrolledWindow-get-capture-button-press?
           GtkScrolledWindow-get-hadjustment
           GtkScrolledWindow-get-hscrollbar
           GtkScrolledWindow-get-kinetic-scrolling?
           GtkScrolledWindow-get-max-content-height
           GtkScrolledWindow-get-max-content-width
           GtkScrolledWindow-get-min-content-height
           GtkScrolledWindow-get-min-content-width
           GtkScrolledWindow-get-overlay-scrolling?
           GtkScrolledWindow-get-placement
           GtkScrolledWindow-get-policy
           GtkScrolledWindow-get-propagate-natural-height?
           GtkScrolledWindow-get-propagate-natural-width?
           GtkScrolledWindow-get-shadow-type
           GtkScrolledWindow-get-vadjustment
           GtkScrolledWindow-get-vscrollbar
           GtkScrolledWindow-new
           GtkScrolledWindow-set-capture-button-press
           GtkScrolledWindow-set-hadjustment
           GtkScrolledWindow-set-kinetic-scrolling
           GtkScrolledWindow-set-max-content-height
           GtkScrolledWindow-set-max-content-width
           GtkScrolledWindow-set-min-content-height
           GtkScrolledWindow-set-min-content-width
           GtkScrolledWindow-set-overlay-scrolling
           GtkScrolledWindow-set-placement
           GtkScrolledWindow-set-policy
           GtkScrolledWindow-set-propagate-natural-height
           GtkScrolledWindow-set-propagate-natural-width
           GtkScrolledWindow-set-shadow-type
           GtkScrolledWindow-set-vadjustment
           GtkScrolledWindow-unset-placement
           GtkSearchBar-connect-entry
           GtkSearchBar-get-search-mode?
           GtkSearchBar-get-show-close-button?
           GtkSearchBar-handle-event?
           GtkSearchBar-new
           GtkSearchBar-set-search-mode
           GtkSearchBar-set-show-close-button
           GtkSearchEntry-handle-event?
           GtkSearchEntry-new
           GtkSelectionData-copy
           GtkSelectionData-free
           GtkSelectionData-get-data
           GtkSelectionData-get-data-type
           GtkSelectionData-get-display
           GtkSelectionData-get-format
           GtkSelectionData-get-length
           GtkSelectionData-get-pixbuf
           GtkSelectionData-get-selection
           GtkSelectionData-get-target
           GtkSelectionData-get-targets?
           GtkSelectionData-get-text
           GtkSelectionData-get-uris
           GtkSelectionData-set
           GtkSelectionData-set-pixbuf?
           GtkSelectionData-set-text?
           GtkSelectionData-set-uris?
           GtkSelectionData-targets-include-image?
           GtkSelectionData-targets-include-rich-text?
           GtkSelectionData-targets-include-text?
           GtkSelectionData-targets-include-uri?
           GtkSeparator-new
           GtkSeparatorMenuItem-new
           GtkSeparatorToolItem-get-draw?
           GtkSeparatorToolItem-new
           GtkSeparatorToolItem-set-draw
           GtkSettings-get-default
           GtkSettings-get-for-screen
           GtkSettings-install-property
           GtkSettings-install-property-parser
           GtkSettings-reset-property
           GtkSettings-set-double-property
           GtkSettings-set-long-property
           GtkSettings-set-property-value
           GtkSettings-set-string-property
           GtkShortcutLabel-get-accelerator
           GtkShortcutLabel-get-disabled-text
           GtkShortcutLabel-new
           GtkShortcutLabel-set-accelerator
           GtkShortcutLabel-set-disabled-text
           GtkSizeGroup-add-widget
           GtkSizeGroup-get-ignore-hidden?
           GtkSizeGroup-get-mode
           GtkSizeGroup-get-widgets
           GtkSizeGroup-new
           GtkSizeGroup-remove-widget
           GtkSizeGroup-set-ignore-hidden
           GtkSizeGroup-set-mode
           GtkSpinButton-configure
           GtkSpinButton-get-adjustment
           GtkSpinButton-get-digits
           GtkSpinButton-get-increments
           GtkSpinButton-get-numeric?
           GtkSpinButton-get-range
           GtkSpinButton-get-snap-to-ticks?
           GtkSpinButton-get-update-policy
           GtkSpinButton-get-value
           GtkSpinButton-get-value-as-int
           GtkSpinButton-get-wrap?
           GtkSpinButton-new
           GtkSpinButton-new-with-range
           GtkSpinButton-set-adjustment
           GtkSpinButton-set-digits
           GtkSpinButton-set-increments
           GtkSpinButton-set-numeric
           GtkSpinButton-set-range
           GtkSpinButton-set-snap-to-ticks
           GtkSpinButton-set-update-policy
           GtkSpinButton-set-value
           GtkSpinButton-set-wrap
           GtkSpinButton-spin
           GtkSpinButton-update
           GtkSpinner-new
           GtkSpinner-start
           GtkSpinner-stop
           GtkStack-add-named
           GtkStack-add-titled
           GtkStack-get-child-by-name
           GtkStack-get-hhomogeneous?
           GtkStack-get-homogeneous?
           GtkStack-get-interpolate-size?
           GtkStack-get-transition-duration
           GtkStack-get-transition-running?
           GtkStack-get-transition-type
           GtkStack-get-vhomogeneous?
           GtkStack-get-visible-child
           GtkStack-get-visible-child-name
           GtkStack-new
           GtkStack-set-hhomogeneous
           GtkStack-set-homogeneous
           GtkStack-set-interpolate-size
           GtkStack-set-transition-duration
           GtkStack-set-transition-type
           GtkStack-set-vhomogeneous
           GtkStack-set-visible-child
           GtkStack-set-visible-child-full
           GtkStack-set-visible-child-name
           GtkStackSidebar-get-stack
           GtkStackSidebar-new
           GtkStackSidebar-set-stack
           GtkStackSwitcher-get-stack
           GtkStackSwitcher-new
           GtkStackSwitcher-set-stack
           GtkStatusIcon-get-geometry?
           GtkStatusIcon-get-gicon
           GtkStatusIcon-get-has-tooltip?
           GtkStatusIcon-get-icon-name
           GtkStatusIcon-get-pixbuf
           GtkStatusIcon-get-screen
           GtkStatusIcon-get-size
           GtkStatusIcon-get-stock
           GtkStatusIcon-get-storage-type
           GtkStatusIcon-get-title
           GtkStatusIcon-get-tooltip-markup
           GtkStatusIcon-get-tooltip-text
           GtkStatusIcon-get-visible?
           GtkStatusIcon-get-x11-window-id
           GtkStatusIcon-is-embedded?
           GtkStatusIcon-new
           GtkStatusIcon-new-from-file
           GtkStatusIcon-new-from-gicon
           GtkStatusIcon-new-from-icon-name
           GtkStatusIcon-new-from-pixbuf
           GtkStatusIcon-new-from-stock
           GtkStatusIcon-position-menu
           GtkStatusIcon-set-from-file
           GtkStatusIcon-set-from-gicon
           GtkStatusIcon-set-from-icon-name
           GtkStatusIcon-set-from-pixbuf
           GtkStatusIcon-set-from-stock
           GtkStatusIcon-set-has-tooltip
           GtkStatusIcon-set-name
           GtkStatusIcon-set-screen
           GtkStatusIcon-set-title
           GtkStatusIcon-set-tooltip-markup
           GtkStatusIcon-set-tooltip-text
           GtkStatusIcon-set-visible
           GtkStatusbar-get-context-id
           GtkStatusbar-get-message-area
           GtkStatusbar-new
           GtkStatusbar-pop
           GtkStatusbar-push
           GtkStatusbar-remove
           GtkStatusbar-remove-all
           GtkStyle-apply-default-background
           GtkStyle-copy
           GtkStyle-detach
           GtkStyle-get-style-property
           GtkStyle-has-context?
           GtkStyle-lookup-color?
           GtkStyle-lookup-icon-set
           GtkStyle-new
           GtkStyle-render-icon
           GtkStyle-set-background
           GtkStyleContext-add-class
           GtkStyleContext-add-provider
           GtkStyleContext-add-provider-for-screen
           GtkStyleContext-add-region
           GtkStyleContext-cancel-animations
           GtkStyleContext-get-background-color
           GtkStyleContext-get-border
           GtkStyleContext-get-border-color
           GtkStyleContext-get-color
           GtkStyleContext-get-direction
           GtkStyleContext-get-font
           GtkStyleContext-get-frame-clock
           GtkStyleContext-get-junction-sides
           GtkStyleContext-get-margin
           GtkStyleContext-get-padding
           GtkStyleContext-get-parent
           GtkStyleContext-get-path
           GtkStyleContext-get-property
           GtkStyleContext-get-scale
           GtkStyleContext-get-screen
           GtkStyleContext-get-section
           GtkStyleContext-get-state
           GtkStyleContext-get-style-property
           GtkStyleContext-has-class?
           GtkStyleContext-has-region?
           GtkStyleContext-invalidate
           GtkStyleContext-list-classes
           GtkStyleContext-list-regions
           GtkStyleContext-lookup-color?
           GtkStyleContext-lookup-icon-set
           GtkStyleContext-new
           GtkStyleContext-notify-state-change
           GtkStyleContext-pop-animatable-region
           GtkStyleContext-push-animatable-region
           GtkStyleContext-remove-class
           GtkStyleContext-remove-provider
           GtkStyleContext-remove-provider-for-screen
           GtkStyleContext-remove-region
           GtkStyleContext-reset-widgets
           GtkStyleContext-restore
           GtkStyleContext-save
           GtkStyleContext-scroll-animations
           GtkStyleContext-set-background
           GtkStyleContext-set-direction
           GtkStyleContext-set-frame-clock
           GtkStyleContext-set-junction-sides
           GtkStyleContext-set-parent
           GtkStyleContext-set-path
           GtkStyleContext-set-scale
           GtkStyleContext-set-screen
           GtkStyleContext-set-state
           GtkStyleContext-state-is-running?
           GtkStyleContext-to-string
           GtkStyleProperties-clear
           GtkStyleProperties-get-property?
           GtkStyleProperties-lookup-color
           GtkStyleProperties-map-color
           GtkStyleProperties-merge
           GtkStyleProperties-new
           GtkStyleProperties-set-property
           GtkStyleProperties-unset-property
           GtkSwitch-get-active?
           GtkSwitch-get-state?
           GtkSwitch-new
           GtkSwitch-set-active
           GtkSwitch-set-state
           GtkSymbolicColor-new-alpha
           GtkSymbolicColor-new-literal
           GtkSymbolicColor-new-mix
           GtkSymbolicColor-new-name
           GtkSymbolicColor-new-shade
           GtkSymbolicColor-new-win32
           GtkSymbolicColor-ref
           GtkSymbolicColor-resolve?
           GtkSymbolicColor-to-string
           GtkSymbolicColor-unref
           GtkTable-attach
           GtkTable-attach-defaults
           GtkTable-get-col-spacing
           GtkTable-get-default-col-spacing
           GtkTable-get-default-row-spacing
           GtkTable-get-homogeneous?
           GtkTable-get-row-spacing
           GtkTable-get-size
           GtkTable-new
           GtkTable-resize
           GtkTable-set-col-spacing
           GtkTable-set-col-spacings
           GtkTable-set-homogeneous
           GtkTable-set-row-spacing
           GtkTable-set-row-spacings
           GtkTargetEntry-copy
           GtkTargetEntry-free
           GtkTargetEntry-new
           GtkTargetList-add
           GtkTargetList-add-image-targets
           GtkTargetList-add-rich-text-targets
           GtkTargetList-add-table
           GtkTargetList-add-text-targets
           GtkTargetList-add-uri-targets
           GtkTargetList-find?
           GtkTargetList-new
           GtkTargetList-ref
           GtkTargetList-remove
           GtkTargetList-unref
           GtkTearoffMenuItem-new
           GtkTextAttributes-copy
           GtkTextAttributes-copy-values
           GtkTextAttributes-new
           GtkTextAttributes-ref
           GtkTextAttributes-unref
           GtkTextBuffer-add-mark
           GtkTextBuffer-add-selection-clipboard
           GtkTextBuffer-apply-tag
           GtkTextBuffer-apply-tag-by-name
           GtkTextBuffer-backspace?
           GtkTextBuffer-begin-user-action
           GtkTextBuffer-copy-clipboard
           GtkTextBuffer-create-child-anchor
           GtkTextBuffer-create-mark
           GtkTextBuffer-cut-clipboard
           GtkTextBuffer-delete
           GtkTextBuffer-delete-interactive?
           GtkTextBuffer-delete-mark
           GtkTextBuffer-delete-mark-by-name
           GtkTextBuffer-delete-selection?
           GtkTextBuffer-deserialize-get-can-create-tags?
           GtkTextBuffer-deserialize-set-can-create-tags
           GtkTextBuffer-deserialize?
           GtkTextBuffer-end-user-action
           GtkTextBuffer-get-bounds
           GtkTextBuffer-get-char-count
           GtkTextBuffer-get-copy-target-list
           GtkTextBuffer-get-deserialize-formats
           GtkTextBuffer-get-end-iter
           GtkTextBuffer-get-has-selection?
           GtkTextBuffer-get-insert
           GtkTextBuffer-get-iter-at-child-anchor
           GtkTextBuffer-get-iter-at-line
           GtkTextBuffer-get-iter-at-line-index
           GtkTextBuffer-get-iter-at-line-offset
           GtkTextBuffer-get-iter-at-mark
           GtkTextBuffer-get-iter-at-offset
           GtkTextBuffer-get-line-count
           GtkTextBuffer-get-mark
           GtkTextBuffer-get-modified?
           GtkTextBuffer-get-paste-target-list
           GtkTextBuffer-get-selection-bound
           GtkTextBuffer-get-selection-bounds?
           GtkTextBuffer-get-serialize-formats
           GtkTextBuffer-get-slice
           GtkTextBuffer-get-start-iter
           GtkTextBuffer-get-tag-table
           GtkTextBuffer-get-text
           GtkTextBuffer-insert
           GtkTextBuffer-insert-at-cursor
           GtkTextBuffer-insert-child-anchor
           GtkTextBuffer-insert-interactive-at-cursor?
           GtkTextBuffer-insert-interactive?
           GtkTextBuffer-insert-markup
           GtkTextBuffer-insert-pixbuf
           GtkTextBuffer-insert-range
           GtkTextBuffer-insert-range-interactive?
           GtkTextBuffer-move-mark
           GtkTextBuffer-move-mark-by-name
           GtkTextBuffer-new
           GtkTextBuffer-paste-clipboard
           GtkTextBuffer-place-cursor
           GtkTextBuffer-register-deserialize-format
           GtkTextBuffer-register-deserialize-tagset
           GtkTextBuffer-register-serialize-format
           GtkTextBuffer-register-serialize-tagset
           GtkTextBuffer-remove-all-tags
           GtkTextBuffer-remove-selection-clipboard
           GtkTextBuffer-remove-tag
           GtkTextBuffer-remove-tag-by-name
           GtkTextBuffer-select-range
           GtkTextBuffer-serialize
           GtkTextBuffer-set-modified
           GtkTextBuffer-set-text
           GtkTextBuffer-unregister-deserialize-format
           GtkTextBuffer-unregister-serialize-format
           GtkTextChildAnchor-get-deleted?
           GtkTextChildAnchor-get-widgets
           GtkTextChildAnchor-new
           GtkTextIter-assign
           GtkTextIter-backward-char?
           GtkTextIter-backward-chars?
           GtkTextIter-backward-cursor-position?
           GtkTextIter-backward-cursor-positions?
           GtkTextIter-backward-find-char?
           GtkTextIter-backward-line?
           GtkTextIter-backward-lines?
           GtkTextIter-backward-search?
           GtkTextIter-backward-sentence-start?
           GtkTextIter-backward-sentence-starts?
           GtkTextIter-backward-to-tag-toggle?
           GtkTextIter-backward-visible-cursor-position?
           GtkTextIter-backward-visible-cursor-positions?
           GtkTextIter-backward-visible-line?
           GtkTextIter-backward-visible-lines?
           GtkTextIter-backward-visible-word-start?
           GtkTextIter-backward-visible-word-starts?
           GtkTextIter-backward-word-start?
           GtkTextIter-backward-word-starts?
           GtkTextIter-begins-tag?
           GtkTextIter-can-insert?
           GtkTextIter-compare
           GtkTextIter-copy
           GtkTextIter-editable?
           GtkTextIter-ends-line?
           GtkTextIter-ends-sentence?
           GtkTextIter-ends-tag?
           GtkTextIter-ends-word?
           GtkTextIter-equal?
           GtkTextIter-forward-char?
           GtkTextIter-forward-chars?
           GtkTextIter-forward-cursor-position?
           GtkTextIter-forward-cursor-positions?
           GtkTextIter-forward-find-char?
           GtkTextIter-forward-line?
           GtkTextIter-forward-lines?
           GtkTextIter-forward-search?
           GtkTextIter-forward-sentence-end?
           GtkTextIter-forward-sentence-ends?
           GtkTextIter-forward-to-end
           GtkTextIter-forward-to-line-end?
           GtkTextIter-forward-to-tag-toggle?
           GtkTextIter-forward-visible-cursor-position?
           GtkTextIter-forward-visible-cursor-positions?
           GtkTextIter-forward-visible-line?
           GtkTextIter-forward-visible-lines?
           GtkTextIter-forward-visible-word-end?
           GtkTextIter-forward-visible-word-ends?
           GtkTextIter-forward-word-end?
           GtkTextIter-forward-word-ends?
           GtkTextIter-free
           GtkTextIter-get-attributes?
           GtkTextIter-get-buffer
           GtkTextIter-get-bytes-in-line
           GtkTextIter-get-char
           GtkTextIter-get-chars-in-line
           GtkTextIter-get-child-anchor
           GtkTextIter-get-language
           GtkTextIter-get-line
           GtkTextIter-get-line-index
           GtkTextIter-get-line-offset
           GtkTextIter-get-marks
           GtkTextIter-get-offset
           GtkTextIter-get-pixbuf
           GtkTextIter-get-slice
           GtkTextIter-get-tags
           GtkTextIter-get-text
           GtkTextIter-get-toggled-tags
           GtkTextIter-get-visible-line-index
           GtkTextIter-get-visible-line-offset
           GtkTextIter-get-visible-slice
           GtkTextIter-get-visible-text
           GtkTextIter-has-tag?
           GtkTextIter-in-range?
           GtkTextIter-inside-sentence?
           GtkTextIter-inside-word?
           GtkTextIter-is-cursor-position?
           GtkTextIter-is-end?
           GtkTextIter-is-start?
           GtkTextIter-order
           GtkTextIter-set-line
           GtkTextIter-set-line-index
           GtkTextIter-set-line-offset
           GtkTextIter-set-offset
           GtkTextIter-set-visible-line-index
           GtkTextIter-set-visible-line-offset
           GtkTextIter-starts-line?
           GtkTextIter-starts-sentence?
           GtkTextIter-starts-tag?
           GtkTextIter-starts-word?
           GtkTextIter-toggles-tag?
           GtkTextMark-get-buffer
           GtkTextMark-get-deleted?
           GtkTextMark-get-left-gravity?
           GtkTextMark-get-name
           GtkTextMark-get-visible?
           GtkTextMark-new
           GtkTextMark-set-visible
           GtkTextTag-changed
           GtkTextTag-event?
           GtkTextTag-get-priority
           GtkTextTag-new
           GtkTextTag-set-priority
           GtkTextTagTable-add?
           GtkTextTagTable-foreach
           GtkTextTagTable-get-size
           GtkTextTagTable-lookup
           GtkTextTagTable-new
           GtkTextTagTable-remove
           GtkTextView-add-child-at-anchor
           GtkTextView-add-child-in-window
           GtkTextView-backward-display-line-start?
           GtkTextView-backward-display-line?
           GtkTextView-buffer-to-window-coords
           GtkTextView-forward-display-line-end?
           GtkTextView-forward-display-line?
           GtkTextView-get-accepts-tab?
           GtkTextView-get-border-window-size
           GtkTextView-get-bottom-margin
           GtkTextView-get-buffer
           GtkTextView-get-cursor-locations
           GtkTextView-get-cursor-visible?
           GtkTextView-get-default-attributes
           GtkTextView-get-editable?
           GtkTextView-get-hadjustment
           GtkTextView-get-indent
           GtkTextView-get-input-hints
           GtkTextView-get-input-purpose
           GtkTextView-get-iter-at-location?
           GtkTextView-get-iter-at-position?
           GtkTextView-get-iter-location
           GtkTextView-get-justification
           GtkTextView-get-left-margin
           GtkTextView-get-line-at-y
           GtkTextView-get-line-yrange
           GtkTextView-get-monospace?
           GtkTextView-get-overwrite?
           GtkTextView-get-pixels-above-lines
           GtkTextView-get-pixels-below-lines
           GtkTextView-get-pixels-inside-wrap
           GtkTextView-get-right-margin
           GtkTextView-get-tabs
           GtkTextView-get-top-margin
           GtkTextView-get-vadjustment
           GtkTextView-get-visible-rect
           GtkTextView-get-window
           GtkTextView-get-window-type
           GtkTextView-get-wrap-mode
           GtkTextView-im-context-filter-keypress?
           GtkTextView-move-child
           GtkTextView-move-mark-onscreen?
           GtkTextView-move-visually?
           GtkTextView-new
           GtkTextView-new-with-buffer
           GtkTextView-place-cursor-onscreen?
           GtkTextView-reset-cursor-blink
           GtkTextView-reset-im-context
           GtkTextView-scroll-mark-onscreen
           GtkTextView-scroll-to-iter?
           GtkTextView-scroll-to-mark
           GtkTextView-set-accepts-tab
           GtkTextView-set-border-window-size
           GtkTextView-set-bottom-margin
           GtkTextView-set-buffer
           GtkTextView-set-cursor-visible
           GtkTextView-set-editable
           GtkTextView-set-indent
           GtkTextView-set-input-hints
           GtkTextView-set-input-purpose
           GtkTextView-set-justification
           GtkTextView-set-left-margin
           GtkTextView-set-monospace
           GtkTextView-set-overwrite
           GtkTextView-set-pixels-above-lines
           GtkTextView-set-pixels-below-lines
           GtkTextView-set-pixels-inside-wrap
           GtkTextView-set-right-margin
           GtkTextView-set-tabs
           GtkTextView-set-top-margin
           GtkTextView-set-wrap-mode
           GtkTextView-starts-display-line?
           GtkTextView-window-to-buffer-coords
           GtkThemingEngine-get-background-color
           GtkThemingEngine-get-border
           GtkThemingEngine-get-border-color
           GtkThemingEngine-get-color
           GtkThemingEngine-get-direction
           GtkThemingEngine-get-font
           GtkThemingEngine-get-junction-sides
           GtkThemingEngine-get-margin
           GtkThemingEngine-get-padding
           GtkThemingEngine-get-path
           GtkThemingEngine-get-property
           GtkThemingEngine-get-screen
           GtkThemingEngine-get-state
           GtkThemingEngine-get-style-property
           GtkThemingEngine-has-class?
           GtkThemingEngine-has-region?
           GtkThemingEngine-load
           GtkThemingEngine-lookup-color?
           GtkThemingEngine-state-is-running?
           GtkToggleAction-get-active?
           GtkToggleAction-get-draw-as-radio?
           GtkToggleAction-new
           GtkToggleAction-set-active
           GtkToggleAction-set-draw-as-radio
           GtkToggleAction-toggled
           GtkToggleButton-get-active?
           GtkToggleButton-get-inconsistent?
           GtkToggleButton-get-mode?
           GtkToggleButton-new
           GtkToggleButton-new-with-label
           GtkToggleButton-new-with-mnemonic
           GtkToggleButton-set-active
           GtkToggleButton-set-inconsistent
           GtkToggleButton-set-mode
           GtkToggleButton-toggled
           GtkToggleToolButton-get-active?
           GtkToggleToolButton-new
           GtkToggleToolButton-new-from-stock
           GtkToggleToolButton-set-active
           GtkToolButton-get-icon-name
           GtkToolButton-get-icon-widget
           GtkToolButton-get-label
           GtkToolButton-get-label-widget
           GtkToolButton-get-stock-id
           GtkToolButton-get-use-underline?
           GtkToolButton-new
           GtkToolButton-new-from-stock
           GtkToolButton-set-icon-name
           GtkToolButton-set-icon-widget
           GtkToolButton-set-label
           GtkToolButton-set-label-widget
           GtkToolButton-set-stock-id
           GtkToolButton-set-use-underline
           GtkToolItem-get-ellipsize-mode
           GtkToolItem-get-expand?
           GtkToolItem-get-homogeneous?
           GtkToolItem-get-icon-size
           GtkToolItem-get-is-important?
           GtkToolItem-get-orientation
           GtkToolItem-get-proxy-menu-item
           GtkToolItem-get-relief-style
           GtkToolItem-get-text-alignment
           GtkToolItem-get-text-orientation
           GtkToolItem-get-text-size-group
           GtkToolItem-get-toolbar-style
           GtkToolItem-get-use-drag-window?
           GtkToolItem-get-visible-horizontal?
           GtkToolItem-get-visible-vertical?
           GtkToolItem-new
           GtkToolItem-rebuild-menu
           GtkToolItem-retrieve-proxy-menu-item
           GtkToolItem-set-expand
           GtkToolItem-set-homogeneous
           GtkToolItem-set-is-important
           GtkToolItem-set-proxy-menu-item
           GtkToolItem-set-tooltip-markup
           GtkToolItem-set-tooltip-text
           GtkToolItem-set-use-drag-window
           GtkToolItem-set-visible-horizontal
           GtkToolItem-set-visible-vertical
           GtkToolItem-toolbar-reconfigured
           GtkToolItemGroup-get-collapsed?
           GtkToolItemGroup-get-drop-item
           GtkToolItemGroup-get-ellipsize
           GtkToolItemGroup-get-header-relief
           GtkToolItemGroup-get-item-position
           GtkToolItemGroup-get-label
           GtkToolItemGroup-get-label-widget
           GtkToolItemGroup-get-n-items
           GtkToolItemGroup-get-nth-item
           GtkToolItemGroup-insert
           GtkToolItemGroup-new
           GtkToolItemGroup-set-collapsed
           GtkToolItemGroup-set-ellipsize
           GtkToolItemGroup-set-header-relief
           GtkToolItemGroup-set-item-position
           GtkToolItemGroup-set-label
           GtkToolItemGroup-set-label-widget
           GtkToolPalette-add-drag-dest
           GtkToolPalette-get-drag-item
           GtkToolPalette-get-drag-target-group
           GtkToolPalette-get-drag-target-item
           GtkToolPalette-get-drop-group
           GtkToolPalette-get-drop-item
           GtkToolPalette-get-exclusive?
           GtkToolPalette-get-expand?
           GtkToolPalette-get-group-position
           GtkToolPalette-get-hadjustment
           GtkToolPalette-get-icon-size
           GtkToolPalette-get-style
           GtkToolPalette-get-vadjustment
           GtkToolPalette-new
           GtkToolPalette-set-drag-source
           GtkToolPalette-set-exclusive
           GtkToolPalette-set-expand
           GtkToolPalette-set-group-position
           GtkToolPalette-set-icon-size
           GtkToolPalette-set-style
           GtkToolPalette-unset-icon-size
           GtkToolPalette-unset-style
           GtkToolbar-get-drop-index
           GtkToolbar-get-icon-size
           GtkToolbar-get-item-index
           GtkToolbar-get-n-items
           GtkToolbar-get-nth-item
           GtkToolbar-get-relief-style
           GtkToolbar-get-show-arrow?
           GtkToolbar-get-style
           GtkToolbar-insert
           GtkToolbar-new
           GtkToolbar-set-drop-highlight-item
           GtkToolbar-set-icon-size
           GtkToolbar-set-show-arrow
           GtkToolbar-set-style
           GtkToolbar-unset-icon-size
           GtkToolbar-unset-style
           GtkTooltip-set-custom
           GtkTooltip-set-icon
           GtkTooltip-set-icon-from-gicon
           GtkTooltip-set-icon-from-icon-name
           GtkTooltip-set-icon-from-stock
           GtkTooltip-set-markup
           GtkTooltip-set-text
           GtkTooltip-set-tip-area
           GtkTooltip-trigger-tooltip-query
           GtkToplevelAccessible-get-children
           GtkTreeIter-copy
           GtkTreeIter-free
           GtkTreeModelFilter-clear-cache
           GtkTreeModelFilter-convert-child-iter-to-iter?
           GtkTreeModelFilter-convert-child-path-to-path
           GtkTreeModelFilter-convert-iter-to-child-iter
           GtkTreeModelFilter-convert-path-to-child-path
           GtkTreeModelFilter-get-model
           GtkTreeModelFilter-refilter
           GtkTreeModelFilter-set-modify-func
           GtkTreeModelFilter-set-visible-column
           GtkTreeModelFilter-set-visible-func
           GtkTreeModelSort-clear-cache
           GtkTreeModelSort-convert-child-iter-to-iter?
           GtkTreeModelSort-convert-child-path-to-path
           GtkTreeModelSort-convert-iter-to-child-iter
           GtkTreeModelSort-convert-path-to-child-path
           GtkTreeModelSort-get-model
           GtkTreeModelSort-iter-is-valid?
           GtkTreeModelSort-reset-default-sort-func
           GtkTreePath-append-index
           GtkTreePath-compare
           GtkTreePath-copy
           GtkTreePath-down
           GtkTreePath-free
           GtkTreePath-get-depth
           GtkTreePath-get-indices
           GtkTreePath-is-ancestor?
           GtkTreePath-is-descendant?
           GtkTreePath-new
           GtkTreePath-new-first
           GtkTreePath-new-from-indices
           GtkTreePath-new-from-string
           GtkTreePath-next
           GtkTreePath-prepend-index
           GtkTreePath-prev?
           GtkTreePath-to-string
           GtkTreePath-up?
           GtkTreeRowReference-copy
           GtkTreeRowReference-deleted
           GtkTreeRowReference-free
           GtkTreeRowReference-get-model
           GtkTreeRowReference-get-path
           GtkTreeRowReference-inserted
           GtkTreeRowReference-new
           GtkTreeRowReference-new-proxy
           GtkTreeRowReference-valid?
           GtkTreeSelection-count-selected-rows
           GtkTreeSelection-get-mode
           GtkTreeSelection-get-selected-rows
           GtkTreeSelection-get-selected?
           GtkTreeSelection-get-tree-view
           GtkTreeSelection-iter-is-selected?
           GtkTreeSelection-path-is-selected?
           GtkTreeSelection-select-all
           GtkTreeSelection-select-iter
           GtkTreeSelection-select-path
           GtkTreeSelection-select-range
           GtkTreeSelection-selected-foreach
           GtkTreeSelection-set-mode
           GtkTreeSelection-set-select-function
           GtkTreeSelection-unselect-all
           GtkTreeSelection-unselect-iter
           GtkTreeSelection-unselect-path
           GtkTreeSelection-unselect-range
           GtkTreeStore-append
           GtkTreeStore-clear
           GtkTreeStore-insert
           GtkTreeStore-insert-after
           GtkTreeStore-insert-before
           GtkTreeStore-insert-with-values
           GtkTreeStore-is-ancestor?
           GtkTreeStore-iter-depth
           GtkTreeStore-iter-is-valid?
           GtkTreeStore-move-after
           GtkTreeStore-move-before
           GtkTreeStore-new
           GtkTreeStore-prepend
           GtkTreeStore-remove?
           GtkTreeStore-set
           GtkTreeStore-set-column-types
           GtkTreeStore-set-value
           GtkTreeStore-swap
           GtkTreeView-append-column
           GtkTreeView-collapse-all
           GtkTreeView-collapse-row?
           GtkTreeView-columns-autosize
           GtkTreeView-convert-bin-window-to-tree-coords
           GtkTreeView-convert-bin-window-to-widget-coords
           GtkTreeView-convert-tree-to-bin-window-coords
           GtkTreeView-convert-tree-to-widget-coords
           GtkTreeView-convert-widget-to-bin-window-coords
           GtkTreeView-convert-widget-to-tree-coords
           GtkTreeView-create-row-drag-icon
           GtkTreeView-enable-model-drag-dest
           GtkTreeView-enable-model-drag-source
           GtkTreeView-expand-all
           GtkTreeView-expand-row?
           GtkTreeView-expand-to-path
           GtkTreeView-get-activate-on-single-click?
           GtkTreeView-get-background-area
           GtkTreeView-get-bin-window
           GtkTreeView-get-cell-area
           GtkTreeView-get-column
           GtkTreeView-get-columns
           GtkTreeView-get-cursor
           GtkTreeView-get-dest-row-at-pos?
           GtkTreeView-get-drag-dest-row
           GtkTreeView-get-enable-search?
           GtkTreeView-get-enable-tree-lines?
           GtkTreeView-get-expander-column
           GtkTreeView-get-fixed-height-mode?
           GtkTreeView-get-grid-lines
           GtkTreeView-get-hadjustment
           GtkTreeView-get-headers-clickable?
           GtkTreeView-get-headers-visible?
           GtkTreeView-get-hover-expand?
           GtkTreeView-get-hover-selection?
           GtkTreeView-get-level-indentation
           GtkTreeView-get-model
           GtkTreeView-get-n-columns
           GtkTreeView-get-path-at-pos?
           GtkTreeView-get-reorderable?
           GtkTreeView-get-rubber-banding?
           GtkTreeView-get-rules-hint?
           GtkTreeView-get-search-column
           GtkTreeView-get-search-entry
           GtkTreeView-get-selection
           GtkTreeView-get-show-expanders?
           GtkTreeView-get-tooltip-column
           GtkTreeView-get-tooltip-context?
           GtkTreeView-get-vadjustment
           GtkTreeView-get-visible-range?
           GtkTreeView-get-visible-rect
           GtkTreeView-insert-column
           GtkTreeView-insert-column-with-data-func
           GtkTreeView-is-blank-at-pos?
           GtkTreeView-is-rubber-banding-active?
           GtkTreeView-map-expanded-rows
           GtkTreeView-move-column-after
           GtkTreeView-new
           GtkTreeView-new-with-model
           GtkTreeView-remove-column
           GtkTreeView-row-activated
           GtkTreeView-row-expanded?
           GtkTreeView-scroll-to-cell
           GtkTreeView-scroll-to-point
           GtkTreeView-set-activate-on-single-click
           GtkTreeView-set-column-drag-function
           GtkTreeView-set-cursor
           GtkTreeView-set-cursor-on-cell
           GtkTreeView-set-destroy-count-func
           GtkTreeView-set-drag-dest-row
           GtkTreeView-set-enable-search
           GtkTreeView-set-enable-tree-lines
           GtkTreeView-set-expander-column
           GtkTreeView-set-fixed-height-mode
           GtkTreeView-set-grid-lines
           GtkTreeView-set-hadjustment
           GtkTreeView-set-headers-clickable
           GtkTreeView-set-headers-visible
           GtkTreeView-set-hover-expand
           GtkTreeView-set-hover-selection
           GtkTreeView-set-level-indentation
           GtkTreeView-set-model
           GtkTreeView-set-reorderable
           GtkTreeView-set-row-separator-func
           GtkTreeView-set-rubber-banding
           GtkTreeView-set-rules-hint
           GtkTreeView-set-search-column
           GtkTreeView-set-search-entry
           GtkTreeView-set-search-equal-func
           GtkTreeView-set-search-position-func
           GtkTreeView-set-show-expanders
           GtkTreeView-set-tooltip-cell
           GtkTreeView-set-tooltip-column
           GtkTreeView-set-tooltip-row
           GtkTreeView-set-vadjustment
           GtkTreeView-unset-rows-drag-dest
           GtkTreeView-unset-rows-drag-source
           GtkTreeViewColumn-add-attribute
           GtkTreeViewColumn-cell-get-position?
           GtkTreeViewColumn-cell-get-size
           GtkTreeViewColumn-cell-is-visible?
           GtkTreeViewColumn-cell-set-cell-data
           GtkTreeViewColumn-clear
           GtkTreeViewColumn-clear-attributes
           GtkTreeViewColumn-clicked
           GtkTreeViewColumn-focus-cell
           GtkTreeViewColumn-get-alignment
           GtkTreeViewColumn-get-button
           GtkTreeViewColumn-get-clickable?
           GtkTreeViewColumn-get-expand?
           GtkTreeViewColumn-get-fixed-width
           GtkTreeViewColumn-get-max-width
           GtkTreeViewColumn-get-min-width
           GtkTreeViewColumn-get-reorderable?
           GtkTreeViewColumn-get-resizable?
           GtkTreeViewColumn-get-sizing
           GtkTreeViewColumn-get-sort-column-id
           GtkTreeViewColumn-get-sort-indicator?
           GtkTreeViewColumn-get-sort-order
           GtkTreeViewColumn-get-spacing
           GtkTreeViewColumn-get-title
           GtkTreeViewColumn-get-tree-view
           GtkTreeViewColumn-get-visible?
           GtkTreeViewColumn-get-widget
           GtkTreeViewColumn-get-width
           GtkTreeViewColumn-get-x-offset
           GtkTreeViewColumn-new
           GtkTreeViewColumn-new-with-area
           GtkTreeViewColumn-pack-end
           GtkTreeViewColumn-pack-start
           GtkTreeViewColumn-queue-resize
           GtkTreeViewColumn-set-alignment
           GtkTreeViewColumn-set-cell-data-func
           GtkTreeViewColumn-set-clickable
           GtkTreeViewColumn-set-expand
           GtkTreeViewColumn-set-fixed-width
           GtkTreeViewColumn-set-max-width
           GtkTreeViewColumn-set-min-width
           GtkTreeViewColumn-set-reorderable
           GtkTreeViewColumn-set-resizable
           GtkTreeViewColumn-set-sizing
           GtkTreeViewColumn-set-sort-column-id
           GtkTreeViewColumn-set-sort-indicator
           GtkTreeViewColumn-set-sort-order
           GtkTreeViewColumn-set-spacing
           GtkTreeViewColumn-set-title
           GtkTreeViewColumn-set-visible
           GtkTreeViewColumn-set-widget
           GtkUIManager-add-ui
           GtkUIManager-add-ui-from-file
           GtkUIManager-add-ui-from-resource
           GtkUIManager-add-ui-from-string
           GtkUIManager-ensure-update
           GtkUIManager-get-accel-group
           GtkUIManager-get-action
           GtkUIManager-get-action-groups
           GtkUIManager-get-add-tearoffs?
           GtkUIManager-get-toplevels
           GtkUIManager-get-ui
           GtkUIManager-get-widget
           GtkUIManager-insert-action-group
           GtkUIManager-new
           GtkUIManager-new-merge-id
           GtkUIManager-remove-action-group
           GtkUIManager-remove-ui
           GtkUIManager-set-add-tearoffs
           GtkVBox-new
           GtkVButtonBox-new
           GtkVPaned-new
           GtkVScale-new
           GtkVScale-new-with-range
           GtkVScrollbar-new
           GtkVSeparator-new
           GtkViewport-get-bin-window
           GtkViewport-get-hadjustment
           GtkViewport-get-shadow-type
           GtkViewport-get-vadjustment
           GtkViewport-get-view-window
           GtkViewport-new
           GtkViewport-set-hadjustment
           GtkViewport-set-shadow-type
           GtkViewport-set-vadjustment
           GtkVolumeButton-new
           GtkWidget-activate?
           GtkWidget-add-accelerator
           GtkWidget-add-device-events
           GtkWidget-add-events
           GtkWidget-add-mnemonic-label
           GtkWidget-add-tick-callback
           GtkWidget-can-activate-accel?
           GtkWidget-child-focus?
           GtkWidget-child-notify
           GtkWidget-class-path
           GtkWidget-compute-expand?
           GtkWidget-create-pango-context
           GtkWidget-create-pango-layout
           GtkWidget-destroy
           GtkWidget-destroyed
           GtkWidget-device-is-shadowed?
           GtkWidget-drag-begin
           GtkWidget-drag-begin-with-coordinates
           GtkWidget-drag-check-threshold?
           GtkWidget-drag-dest-add-image-targets
           GtkWidget-drag-dest-add-text-targets
           GtkWidget-drag-dest-add-uri-targets
           GtkWidget-drag-dest-find-target
           GtkWidget-drag-dest-get-target-list
           GtkWidget-drag-dest-get-track-motion?
           GtkWidget-drag-dest-set
           GtkWidget-drag-dest-set-proxy
           GtkWidget-drag-dest-set-target-list
           GtkWidget-drag-dest-set-track-motion
           GtkWidget-drag-dest-unset
           GtkWidget-drag-get-data
           GtkWidget-drag-highlight
           GtkWidget-drag-source-add-image-targets
           GtkWidget-drag-source-add-text-targets
           GtkWidget-drag-source-add-uri-targets
           GtkWidget-drag-source-get-target-list
           GtkWidget-drag-source-set
           GtkWidget-drag-source-set-icon-gicon
           GtkWidget-drag-source-set-icon-name
           GtkWidget-drag-source-set-icon-pixbuf
           GtkWidget-drag-source-set-icon-stock
           GtkWidget-drag-source-set-target-list
           GtkWidget-drag-source-unset
           GtkWidget-drag-unhighlight
           GtkWidget-draw
           GtkWidget-ensure-style
           GtkWidget-error-bell
           GtkWidget-event?
           GtkWidget-freeze-child-notify
           GtkWidget-get-accessible
           GtkWidget-get-action-group
           GtkWidget-get-allocated-baseline
           GtkWidget-get-allocated-height
           GtkWidget-get-allocated-size
           GtkWidget-get-allocated-width
           GtkWidget-get-allocation
           GtkWidget-get-ancestor
           GtkWidget-get-app-paintable?
           GtkWidget-get-can-default?
           GtkWidget-get-can-focus?
           GtkWidget-get-child-requisition
           GtkWidget-get-child-visible?
           GtkWidget-get-clip
           GtkWidget-get-clipboard
           GtkWidget-get-composite-name
           GtkWidget-get-default-direction
           GtkWidget-get-default-style
           GtkWidget-get-device-enabled?
           GtkWidget-get-device-events
           GtkWidget-get-direction
           GtkWidget-get-display
           GtkWidget-get-double-buffered?
           GtkWidget-get-events
           GtkWidget-get-focus-on-click?
           GtkWidget-get-font-map
           GtkWidget-get-font-options
           GtkWidget-get-frame-clock
           GtkWidget-get-halign
           GtkWidget-get-has-tooltip?
           GtkWidget-get-has-window?
           GtkWidget-get-hexpand-set?
           GtkWidget-get-hexpand?
           GtkWidget-get-mapped?
           GtkWidget-get-margin-bottom
           GtkWidget-get-margin-end
           GtkWidget-get-margin-left
           GtkWidget-get-margin-right
           GtkWidget-get-margin-start
           GtkWidget-get-margin-top
           GtkWidget-get-modifier-mask
           GtkWidget-get-modifier-style
           GtkWidget-get-name
           GtkWidget-get-no-show-all?
           GtkWidget-get-opacity
           GtkWidget-get-pango-context
           GtkWidget-get-parent
           GtkWidget-get-parent-window
           GtkWidget-get-path
           GtkWidget-get-pointer
           GtkWidget-get-preferred-height
           GtkWidget-get-preferred-height-and-baseline-for-width
           GtkWidget-get-preferred-height-for-width
           GtkWidget-get-preferred-size
           GtkWidget-get-preferred-width
           GtkWidget-get-preferred-width-for-height
           GtkWidget-get-realized?
           GtkWidget-get-receives-default?
           GtkWidget-get-request-mode
           GtkWidget-get-requisition
           GtkWidget-get-root-window
           GtkWidget-get-scale-factor
           GtkWidget-get-screen
           GtkWidget-get-sensitive?
           GtkWidget-get-settings
           GtkWidget-get-size-request
           GtkWidget-get-state
           GtkWidget-get-state-flags
           GtkWidget-get-style
           GtkWidget-get-style-context
           GtkWidget-get-support-multidevice?
           GtkWidget-get-template-child
           GtkWidget-get-tooltip-markup
           GtkWidget-get-tooltip-text
           GtkWidget-get-tooltip-window
           GtkWidget-get-toplevel
           GtkWidget-get-valign
           GtkWidget-get-valign-with-baseline
           GtkWidget-get-vexpand-set?
           GtkWidget-get-vexpand?
           GtkWidget-get-visible?
           GtkWidget-get-visual
           GtkWidget-get-window
           GtkWidget-grab-add
           GtkWidget-grab-default
           GtkWidget-grab-focus
           GtkWidget-grab-remove
           GtkWidget-has-default?
           GtkWidget-has-focus?
           GtkWidget-has-grab?
           GtkWidget-has-rc-style?
           GtkWidget-has-screen?
           GtkWidget-has-visible-focus?
           GtkWidget-hide
           GtkWidget-hide-on-delete?
           GtkWidget-in-destruction?
           GtkWidget-init-template
           GtkWidget-input-shape-combine-region
           GtkWidget-insert-action-group
           GtkWidget-intersect?
           GtkWidget-is-ancestor?
           GtkWidget-is-composited?
           GtkWidget-is-drawable?
           GtkWidget-is-focus?
           GtkWidget-is-sensitive?
           GtkWidget-is-toplevel?
           GtkWidget-is-visible?
           GtkWidget-keynav-failed?
           GtkWidget-list-accel-closures
           GtkWidget-list-action-prefixes
           GtkWidget-list-mnemonic-labels
           GtkWidget-map
           GtkWidget-mnemonic-activate?
           GtkWidget-modify-base
           GtkWidget-modify-bg
           GtkWidget-modify-cursor
           GtkWidget-modify-fg
           GtkWidget-modify-font
           GtkWidget-modify-style
           GtkWidget-modify-text
           GtkWidget-override-background-color
           GtkWidget-override-color
           GtkWidget-override-cursor
           GtkWidget-override-font
           GtkWidget-override-symbolic-color
           GtkWidget-path
           GtkWidget-pop-composite-child
           GtkWidget-push-composite-child
           GtkWidget-queue-allocate
           GtkWidget-queue-compute-expand
           GtkWidget-queue-draw
           GtkWidget-queue-draw-area
           GtkWidget-queue-draw-region
           GtkWidget-queue-resize
           GtkWidget-queue-resize-no-redraw
           GtkWidget-realize
           GtkWidget-region-intersect
           GtkWidget-register-window
           GtkWidget-remove-accelerator?
           GtkWidget-remove-mnemonic-label
           GtkWidget-remove-tick-callback
           GtkWidget-render-icon
           GtkWidget-render-icon-pixbuf
           GtkWidget-reparent
           GtkWidget-reset-rc-styles
           GtkWidget-reset-style
           GtkWidget-send-expose
           GtkWidget-send-focus-change?
           GtkWidget-set-accel-path
           GtkWidget-set-allocation
           GtkWidget-set-app-paintable
           GtkWidget-set-can-default
           GtkWidget-set-can-focus
           GtkWidget-set-child-visible
           GtkWidget-set-clip
           GtkWidget-set-composite-name
           GtkWidget-set-default-direction
           GtkWidget-set-device-enabled
           GtkWidget-set-device-events
           GtkWidget-set-direction
           GtkWidget-set-double-buffered
           GtkWidget-set-events
           GtkWidget-set-focus-on-click
           GtkWidget-set-font-map
           GtkWidget-set-font-options
           GtkWidget-set-halign
           GtkWidget-set-has-tooltip
           GtkWidget-set-has-window
           GtkWidget-set-hexpand
           GtkWidget-set-hexpand-set
           GtkWidget-set-mapped
           GtkWidget-set-margin-bottom
           GtkWidget-set-margin-end
           GtkWidget-set-margin-left
           GtkWidget-set-margin-right
           GtkWidget-set-margin-start
           GtkWidget-set-margin-top
           GtkWidget-set-name
           GtkWidget-set-no-show-all
           GtkWidget-set-opacity
           GtkWidget-set-parent
           GtkWidget-set-parent-window
           GtkWidget-set-realized
           GtkWidget-set-receives-default
           GtkWidget-set-redraw-on-allocate
           GtkWidget-set-sensitive
           GtkWidget-set-size-request
           GtkWidget-set-state
           GtkWidget-set-state-flags
           GtkWidget-set-style
           GtkWidget-set-support-multidevice
           GtkWidget-set-tooltip-markup
           GtkWidget-set-tooltip-text
           GtkWidget-set-tooltip-window
           GtkWidget-set-valign
           GtkWidget-set-vexpand
           GtkWidget-set-vexpand-set
           GtkWidget-set-visible
           GtkWidget-set-visual
           GtkWidget-set-window
           GtkWidget-shape-combine-region
           GtkWidget-show
           GtkWidget-show-all
           GtkWidget-show-now
           GtkWidget-size-allocate
           GtkWidget-size-allocate-with-baseline
           GtkWidget-size-request
           GtkWidget-style-attach
           GtkWidget-style-get-property
           GtkWidget-thaw-child-notify
           GtkWidget-translate-coordinates?
           GtkWidget-trigger-tooltip-query
           GtkWidget-unmap
           GtkWidget-unparent
           GtkWidget-unrealize
           GtkWidget-unregister-window
           GtkWidget-unset-state-flags
           GtkWidgetPath-append-for-widget
           GtkWidgetPath-append-type
           GtkWidgetPath-append-with-siblings
           GtkWidgetPath-copy
           GtkWidgetPath-free
           GtkWidgetPath-get-object-type
           GtkWidgetPath-has-parent?
           GtkWidgetPath-is-type?
           GtkWidgetPath-iter-add-class
           GtkWidgetPath-iter-add-region
           GtkWidgetPath-iter-clear-classes
           GtkWidgetPath-iter-clear-regions
           GtkWidgetPath-iter-get-name
           GtkWidgetPath-iter-get-object-name
           GtkWidgetPath-iter-get-object-type
           GtkWidgetPath-iter-get-sibling-index
           GtkWidgetPath-iter-get-siblings
           GtkWidgetPath-iter-get-state
           GtkWidgetPath-iter-has-class?
           GtkWidgetPath-iter-has-name?
           GtkWidgetPath-iter-has-qclass?
           GtkWidgetPath-iter-has-qname?
           GtkWidgetPath-iter-has-qregion?
           GtkWidgetPath-iter-has-region?
           GtkWidgetPath-iter-list-classes
           GtkWidgetPath-iter-list-regions
           GtkWidgetPath-iter-remove-class
           GtkWidgetPath-iter-remove-region
           GtkWidgetPath-iter-set-name
           GtkWidgetPath-iter-set-object-name
           GtkWidgetPath-iter-set-object-type
           GtkWidgetPath-iter-set-state
           GtkWidgetPath-length
           GtkWidgetPath-new
           GtkWidgetPath-prepend-type
           GtkWidgetPath-ref
           GtkWidgetPath-to-string
           GtkWidgetPath-unref
           GtkWindow-activate-default?
           GtkWindow-activate-focus?
           GtkWindow-activate-key?
           GtkWindow-add-accel-group
           GtkWindow-add-mnemonic
           GtkWindow-begin-move-drag
           GtkWindow-begin-resize-drag
           GtkWindow-close
           GtkWindow-deiconify
           GtkWindow-fullscreen
           GtkWindow-fullscreen-on-monitor
           GtkWindow-get-accept-focus?
           GtkWindow-get-application
           GtkWindow-get-attached-to
           GtkWindow-get-decorated?
           GtkWindow-get-default-icon-list
           GtkWindow-get-default-icon-name
           GtkWindow-get-default-size
           GtkWindow-get-default-widget
           GtkWindow-get-deletable?
           GtkWindow-get-destroy-with-parent?
           GtkWindow-get-focus
           GtkWindow-get-focus-on-map?
           GtkWindow-get-focus-visible?
           GtkWindow-get-gravity
           GtkWindow-get-group
           GtkWindow-get-has-resize-grip?
           GtkWindow-get-hide-titlebar-when-maximized?
           GtkWindow-get-icon
           GtkWindow-get-icon-list
           GtkWindow-get-icon-name
           GtkWindow-get-mnemonic-modifier
           GtkWindow-get-mnemonics-visible?
           GtkWindow-get-modal?
           GtkWindow-get-opacity
           GtkWindow-get-position
           GtkWindow-get-resizable?
           GtkWindow-get-resize-grip-area?
           GtkWindow-get-role
           GtkWindow-get-screen
           GtkWindow-get-size
           GtkWindow-get-skip-pager-hint?
           GtkWindow-get-skip-taskbar-hint?
           GtkWindow-get-title
           GtkWindow-get-titlebar
           GtkWindow-get-transient-for
           GtkWindow-get-type-hint
           GtkWindow-get-urgency-hint?
           GtkWindow-get-window-type
           GtkWindow-has-group?
           GtkWindow-has-toplevel-focus?
           GtkWindow-iconify
           GtkWindow-is-active?
           GtkWindow-is-maximized?
           GtkWindow-list-toplevels
           GtkWindow-maximize
           GtkWindow-mnemonic-activate?
           GtkWindow-move
           GtkWindow-new
           GtkWindow-parse-geometry?
           GtkWindow-present
           GtkWindow-present-with-time
           GtkWindow-propagate-key-event?
           GtkWindow-remove-accel-group
           GtkWindow-remove-mnemonic
           GtkWindow-reshow-with-initial-size
           GtkWindow-resize
           GtkWindow-resize-grip-is-visible?
           GtkWindow-resize-to-geometry
           GtkWindow-set-accept-focus
           GtkWindow-set-application
           GtkWindow-set-attached-to
           GtkWindow-set-auto-startup-notification
           GtkWindow-set-decorated
           GtkWindow-set-default
           GtkWindow-set-default-geometry
           GtkWindow-set-default-icon
           GtkWindow-set-default-icon-from-file?
           GtkWindow-set-default-icon-list
           GtkWindow-set-default-icon-name
           GtkWindow-set-default-size
           GtkWindow-set-deletable
           GtkWindow-set-destroy-with-parent
           GtkWindow-set-focus
           GtkWindow-set-focus-on-map
           GtkWindow-set-focus-visible
           GtkWindow-set-geometry-hints
           GtkWindow-set-gravity
           GtkWindow-set-has-resize-grip
           GtkWindow-set-has-user-ref-count
           GtkWindow-set-hide-titlebar-when-maximized
           GtkWindow-set-icon
           GtkWindow-set-icon-from-file?
           GtkWindow-set-icon-list
           GtkWindow-set-icon-name
           GtkWindow-set-interactive-debugging
           GtkWindow-set-keep-above
           GtkWindow-set-keep-below
           GtkWindow-set-mnemonic-modifier
           GtkWindow-set-mnemonics-visible
           GtkWindow-set-modal
           GtkWindow-set-opacity
           GtkWindow-set-position
           GtkWindow-set-resizable
           GtkWindow-set-role
           GtkWindow-set-screen
           GtkWindow-set-skip-pager-hint
           GtkWindow-set-skip-taskbar-hint
           GtkWindow-set-startup-id
           GtkWindow-set-title
           GtkWindow-set-titlebar
           GtkWindow-set-transient-for
           GtkWindow-set-type-hint
           GtkWindow-set-urgency-hint
           GtkWindow-set-wmclass
           GtkWindow-stick
           GtkWindow-unfullscreen
           GtkWindow-unmaximize
           GtkWindow-unstick
           GtkWindowGroup-add-window
           GtkWindowGroup-get-current-device-grab
           GtkWindowGroup-get-current-grab
           GtkWindowGroup-list-windows
           GtkWindowGroup-new
           GtkWindowGroup-remove-window
           gtk-accel-groups-activate
           gtk-accel-groups-from-object
           gtk-accelerator-get-default-mod-mask
           gtk-accelerator-get-label
           gtk-accelerator-get-label-with-keycode
           gtk-accelerator-name
           gtk-accelerator-name-with-keycode
           gtk-accelerator-parse
           gtk-accelerator-parse-with-keycode
           gtk-accelerator-set-default-mod-mask
           gtk-accelerator-valid
           gtk-binding-entry-add-signal-from-string
           gtk-binding-entry-add-signall
           gtk-binding-entry-remove
           gtk-binding-entry-skip
           gtk-binding-set-find
           gtk-bindings-activate
           gtk-bindings-activate-event
           gtk-builder-error-quark
           gtk-cairo-should-draw-window
           gtk-cairo-transform-to-window
           gtk-check-version
           gtk-css-provider-error-quark
           gtk-device-grab-add
           gtk-device-grab-remove
           gtk-disable-setlocale
           gtk-distribute-natural-allocation
           gtk-drag-cancel
           gtk-drag-finish
           gtk-drag-get-source-widget
           gtk-drag-set-icon-default
           gtk-drag-set-icon-gicon
           gtk-drag-set-icon-name
           gtk-drag-set-icon-pixbuf
           gtk-drag-set-icon-surface
           gtk-drag-set-icon-widget
           gtk-events-pending
           gtk-false
           gtk-file-chooser-error-quark
           gtk-get-binary-age
           gtk-get-current-event
           gtk-get-current-event-device
           gtk-get-current-event-state
           gtk-get-current-event-time
           gtk-get-debug-flags
           gtk-get-default-language
           gtk-get-event-widget
           gtk-get-interface-age
           gtk-get-locale-direction
           gtk-get-major-version
           gtk-get-micro-version
           gtk-get-minor-version
           gtk-get-option-group
           gtk-grab-get-current
           gtk-icon-size-lookup
           gtk-icon-theme-error-quark
           gtk-init
           gtk-init-abi-check
           gtk-init-check
           gtk-init-check-abi-check
           gtk-init-with-args
           gtk-main
           gtk-main-do-event
           gtk-main-iteration
           gtk-main-iteration-do
           gtk-main-level
           gtk-main-quit
           gtk-paper-size-get-default
           gtk-paper-size-get-paper-sizes
           gtk-parse-args
           gtk-print-error-quark
           gtk-print-run-page-setup-dialog
           gtk-print-run-page-setup-dialog-async
           gtk-propagate-event
           gtk-rc-property-parse-border
           gtk-rc-property-parse-color
           gtk-rc-property-parse-enum
           gtk-rc-property-parse-flags
           gtk-rc-property-parse-requisition
           gtk-recent-chooser-error-quark
           gtk-recent-manager-error-quark
           gtk-render-activity
           gtk-render-arrow
           gtk-render-background
           gtk-render-background-get-clip
           gtk-render-check
           gtk-render-expander
           gtk-render-extension
           gtk-render-focus
           gtk-render-frame
           gtk-render-handle
           gtk-render-icon
           gtk-render-icon-surface
           gtk-render-insertion-cursor
           gtk-render-layout
           gtk-render-line
           gtk-render-option
           gtk-render-slider
           gtk-rgb-to-hsv
           gtk-selection-add-target
           gtk-selection-add-targets
           gtk-selection-clear-targets
           gtk-selection-convert
           gtk-selection-owner-set
           gtk-selection-owner-set-for-display
           gtk-selection-remove-all
           gtk-set-debug-flags
           gtk-show-uri
           gtk-show-uri-on-window
           gtk-target-table-free
           gtk-target-table-new-from-list
           gtk-targets-include-image
           gtk-targets-include-rich-text
           gtk-targets-include-text
           gtk-targets-include-uri
           gtk-test-find-label
           gtk-test-find-sibling
           gtk-test-find-widget
           gtk-test-list-all-types
           gtk-test-register-all-types
           gtk-test-widget-send-key
           gtk-test-widget-wait-for-draw
           gtk-tree-get-row-drag-data
           gtk-tree-row-reference-deleted
           gtk-tree-row-reference-inserted
           gtk-tree-set-row-drag-data
           gtk-true

           ;; Constants
           ACCEL
           ALIGN
           APPLICATION_INHIBIT
           ARROW_PLACEMENT
           ARROW_TYPE
           ASSISTANT_PAGE_TYPE
           ATTACH_OPTIONS
           BASELINE_POSITION
           BINARY_AGE
           BORDER_STYLE
           BUILDER_ERROR
           BUTTONS_TYPE
           BUTTON_BOX_STYLE
           BUTTON_ROLE
           CALENDAR_DISPLAY_OPTIONS
           CELL_RENDERER_ACCEL_MODE
           CELL_RENDERER_MODE
           CELL_RENDERER_STATE
           CORNER_TYPE
           CSS_PROVIDER_ERROR
           CSS_SECTION_TYPE
           DEBUG_FLAG
           DELETE_TYPE
           DEST_DEFAULTS
           DIALOG
           DIRECTION_TYPE
           DRAG_RESULT
           ENTRY_ICON_POSITION
           EVENT_CONTROLLER_SCROLL
           EVENT_SEQUENCE_STATE
           EXPANDER_STYLE
           FILE_CHOOSER_ACTION
           FILE_CHOOSER_CONFIRMATION
           FILE_CHOOSER_ERROR
           FILE_FILTER
           FONT_CHOOSER_LEVEL
           ICON_LOOKUP
           ICON_SIZE
           ICON_THEME_ERROR
           ICON_VIEW_DROP_POSITION
           IMAGE_TYPE
           INPUT_ERROR
           INPUT_HINTS
           INPUT_PURPOSE
           INTERFACE_AGE
           JUNCTION_SIDES
           JUSTIFICATION
           LEVEL_BAR_MODE
           LEVEL_BAR_OFFSET_FULL
           LEVEL_BAR_OFFSET_HIGH
           LEVEL_BAR_OFFSET_LOW
           LICENSE
           MAJOR_VERSION
           MAX_COMPOSE_LEN
           MENU_DIRECTION_TYPE
           MESSAGE_TYPE
           MICRO_VERSION
           MINOR_VERSION
           MOVEMENT_STEP
           NOTEBOOK_TAB
           NUMBER_UP_LAYOUT
           ORIENTATION
           PACK_DIRECTION
           PACK_TYPE
           PAD_ACTION_TYPE
           PAGE_ORIENTATION
           PAGE_SET
           PAN_DIRECTION
           PAPER_NAME_A3
           PAPER_NAME_A4
           PAPER_NAME_A5
           PAPER_NAME_B5
           PAPER_NAME_EXECUTIVE
           PAPER_NAME_LEGAL
           PAPER_NAME_LETTER
           PATH_PRIO_MASK
           PLACES_OPEN
           POLICY_TYPE
           POPOVER_CONSTRAINT
           POSITION_TYPE
           PRINT_DUPLEX
           PRINT_ERROR
           PRINT_OPERATION_ACTION
           PRINT_OPERATION_RESULT
           PRINT_PAGES
           PRINT_QUALITY
           PRINT_SETTINGS_COLLATE
           PRINT_SETTINGS_DEFAULT_SOURCE
           PRINT_SETTINGS_DITHER
           PRINT_SETTINGS_DUPLEX
           PRINT_SETTINGS_FINISHINGS
           PRINT_SETTINGS_MEDIA_TYPE
           PRINT_SETTINGS_NUMBER_UP
           PRINT_SETTINGS_NUMBER_UP_LAYOUT
           PRINT_SETTINGS_N_COPIES
           PRINT_SETTINGS_ORIENTATION
           PRINT_SETTINGS_OUTPUT_BASENAME
           PRINT_SETTINGS_OUTPUT_BIN
           PRINT_SETTINGS_OUTPUT_DIR
           PRINT_SETTINGS_OUTPUT_FILE_FORMAT
           PRINT_SETTINGS_OUTPUT_URI
           PRINT_SETTINGS_PAGE_RANGES
           PRINT_SETTINGS_PAGE_SET
           PRINT_SETTINGS_PAPER_FORMAT
           PRINT_SETTINGS_PAPER_HEIGHT
           PRINT_SETTINGS_PAPER_WIDTH
           PRINT_SETTINGS_PRINTER
           PRINT_SETTINGS_PRINTER_LPI
           PRINT_SETTINGS_PRINT_PAGES
           PRINT_SETTINGS_QUALITY
           PRINT_SETTINGS_RESOLUTION
           PRINT_SETTINGS_RESOLUTION_X
           PRINT_SETTINGS_RESOLUTION_Y
           PRINT_SETTINGS_REVERSE
           PRINT_SETTINGS_SCALE
           PRINT_SETTINGS_USE_COLOR
           PRINT_SETTINGS_WIN32_DRIVER_EXTRA
           PRINT_SETTINGS_WIN32_DRIVER_VERSION
           PRINT_STATUS
           PRIORITY_RESIZE
           PROPAGATION_PHASE
           RC
           RECENT_CHOOSER_ERROR
           RECENT_FILTER
           RECENT_MANAGER_ERROR
           RECENT_SORT_TYPE
           REGION
           RELIEF_STYLE
           RESIZE_MODE
           RESPONSE_TYPE
           REVEALER_TRANSITION_TYPE
           SCROLLABLE_POLICY
           SCROLL_STEP
           SCROLL_TYPE
           SELECTION_MODE
           SENSITIVITY_TYPE
           SHADOW_TYPE
           SHORTCUT_TYPE
           SIZE_GROUP_MODE
           SIZE_REQUEST_MODE
           SORT_TYPE
           SPIN_BUTTON_UPDATE_POLICY
           SPIN_TYPE
           STACK_TRANSITION_TYPE
           STATE
           STYLE_CLASS_ACCELERATOR
           STYLE_CLASS_ARROW
           STYLE_CLASS_BACKGROUND
           STYLE_CLASS_BOTTOM
           STYLE_CLASS_BUTTON
           STYLE_CLASS_CALENDAR
           STYLE_CLASS_CELL
           STYLE_CLASS_CHECK
           STYLE_CLASS_COMBOBOX_ENTRY
           STYLE_CLASS_CONTEXT_MENU
           STYLE_CLASS_CSD
           STYLE_CLASS_CURSOR_HANDLE
           STYLE_CLASS_DEFAULT
           STYLE_CLASS_DESTRUCTIVE_ACTION
           STYLE_CLASS_DIM_LABEL
           STYLE_CLASS_DND
           STYLE_CLASS_DOCK
           STYLE_CLASS_ENTRY
           STYLE_CLASS_ERROR
           STYLE_CLASS_EXPANDER
           STYLE_CLASS_FLAT
           STYLE_CLASS_FRAME
           STYLE_CLASS_GRIP
           STYLE_CLASS_HEADER
           STYLE_CLASS_HIGHLIGHT
           STYLE_CLASS_HORIZONTAL
           STYLE_CLASS_IMAGE
           STYLE_CLASS_INFO
           STYLE_CLASS_INLINE_TOOLBAR
           STYLE_CLASS_INSERTION_CURSOR
           STYLE_CLASS_LABEL
           STYLE_CLASS_LEFT
           STYLE_CLASS_LEVEL_BAR
           STYLE_CLASS_LINKED
           STYLE_CLASS_LIST
           STYLE_CLASS_LIST_ROW
           STYLE_CLASS_MARK
           STYLE_CLASS_MENU
           STYLE_CLASS_MENUBAR
           STYLE_CLASS_MENUITEM
           STYLE_CLASS_MESSAGE_DIALOG
           STYLE_CLASS_MONOSPACE
           STYLE_CLASS_NEEDS_ATTENTION
           STYLE_CLASS_NOTEBOOK
           STYLE_CLASS_OSD
           STYLE_CLASS_OVERSHOOT
           STYLE_CLASS_PANE_SEPARATOR
           STYLE_CLASS_PAPER
           STYLE_CLASS_POPOVER
           STYLE_CLASS_POPUP
           STYLE_CLASS_PRIMARY_TOOLBAR
           STYLE_CLASS_PROGRESSBAR
           STYLE_CLASS_PULSE
           STYLE_CLASS_QUESTION
           STYLE_CLASS_RADIO
           STYLE_CLASS_RAISED
           STYLE_CLASS_READ_ONLY
           STYLE_CLASS_RIGHT
           STYLE_CLASS_RUBBERBAND
           STYLE_CLASS_SCALE
           STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
           STYLE_CLASS_SCALE_HAS_MARKS_BELOW
           STYLE_CLASS_SCROLLBAR
           STYLE_CLASS_SCROLLBARS_JUNCTION
           STYLE_CLASS_SEPARATOR
           STYLE_CLASS_SIDEBAR
           STYLE_CLASS_SLIDER
           STYLE_CLASS_SPINBUTTON
           STYLE_CLASS_SPINNER
           STYLE_CLASS_STATUSBAR
           STYLE_CLASS_SUBTITLE
           STYLE_CLASS_SUGGESTED_ACTION
           STYLE_CLASS_TITLE
           STYLE_CLASS_TITLEBAR
           STYLE_CLASS_TOOLBAR
           STYLE_CLASS_TOOLTIP
           STYLE_CLASS_TOP
           STYLE_CLASS_TOUCH_SELECTION
           STYLE_CLASS_TROUGH
           STYLE_CLASS_UNDERSHOOT
           STYLE_CLASS_VERTICAL
           STYLE_CLASS_VIEW
           STYLE_CLASS_WARNING
           STYLE_CLASS_WIDE
           STYLE_CONTEXT_PRINT
           STYLE_PROPERTY_BACKGROUND_COLOR
           STYLE_PROPERTY_BACKGROUND_IMAGE
           STYLE_PROPERTY_BORDER_COLOR
           STYLE_PROPERTY_BORDER_RADIUS
           STYLE_PROPERTY_BORDER_STYLE
           STYLE_PROPERTY_BORDER_WIDTH
           STYLE_PROPERTY_COLOR
           STYLE_PROPERTY_FONT
           STYLE_PROPERTY_MARGIN
           STYLE_PROPERTY_PADDING
           STYLE_PROVIDER_PRIORITY_APPLICATION
           STYLE_PROVIDER_PRIORITY_FALLBACK
           STYLE_PROVIDER_PRIORITY_SETTINGS
           STYLE_PROVIDER_PRIORITY_THEME
           STYLE_PROVIDER_PRIORITY_USER
           TARGET
           TEXT_BUFFER_TARGET_INFO
           TEXT_DIRECTION
           TEXT_EXTEND_SELECTION
           TEXT_SEARCH
           TEXT_VIEW_LAYER
           TEXT_VIEW_PRIORITY_VALIDATE
           TEXT_WINDOW_TYPE
           TOOLBAR_STYLE
           TOOL_PALETTE_DRAG_TARGETS
           TREE_MODEL
           TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
           TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
           TREE_VIEW_COLUMN_SIZING
           TREE_VIEW_DROP_POSITION
           TREE_VIEW_GRID_LINES
           UNIT
           WIDGET_HELP_TYPE
           WINDOW_POSITION
           WINDOW_TYPE
           WRAP_MODE

           ;; Types
           <GtkAboutDialog>
           <GtkAccelGroup>
           <GtkAccelLabel>
           <GtkAccelMap>
           <GtkAccessible>
           <GtkAction>
           <GtkActionBar>
           <GtkActionGroup>
           <GtkAdjustment>
           <GtkAlignment>
           <GtkAppChooserButton>
           <GtkAppChooserDialog>
           <GtkAppChooserWidget>
           <GtkApplication>
           <GtkApplicationWindow>
           <GtkArrow>
           <GtkArrowAccessible>
           <GtkAspectFrame>
           <GtkAssistant>
           <GtkBin>
           <GtkBooleanCellAccessible>
           <GtkBorder>
           <GtkBox>
           <GtkBuilder>
           <GtkButton>
           <GtkButtonAccessible>
           <GtkButtonBox>
           <GtkCalendar>
           <GtkCellAccessible>
           <GtkCellArea>
           <GtkCellAreaBox>
           <GtkCellAreaContext>
           <GtkCellRenderer>
           <GtkCellRendererAccel>
           <GtkCellRendererCombo>
           <GtkCellRendererPixbuf>
           <GtkCellRendererProgress>
           <GtkCellRendererSpin>
           <GtkCellRendererSpinner>
           <GtkCellRendererText>
           <GtkCellRendererToggle>
           <GtkCellView>
           <GtkCheckButton>
           <GtkCheckMenuItem>
           <GtkCheckMenuItemAccessible>
           <GtkClipboard>
           <GtkColorButton>
           <GtkColorChooserDialog>
           <GtkColorChooserWidget>
           <GtkColorSelection>
           <GtkColorSelectionDialog>
           <GtkComboBox>
           <GtkComboBoxAccessible>
           <GtkComboBoxText>
           <GtkContainer>
           <GtkContainerAccessible>
           <GtkContainerCellAccessible>
           <GtkCssProvider>
           <GtkCssSection>
           <GtkDialog>
           <GtkDrawingArea>
           <GtkEntry>
           <GtkEntryAccessible>
           <GtkEntryBuffer>
           <GtkEntryCompletion>
           <GtkEntryIconAccessible>
           <GtkEventBox>
           <GtkEventController>
           <GtkEventControllerKey>
           <GtkEventControllerMotion>
           <GtkEventControllerScroll>
           <GtkExpander>
           <GtkExpanderAccessible>
           <GtkFileChooserButton>
           <GtkFileChooserDialog>
           <GtkFileChooserNative>
           <GtkFileChooserWidget>
           <GtkFileFilter>
           <GtkFixed>
           <GtkFlowBox>
           <GtkFlowBoxAccessible>
           <GtkFlowBoxChild>
           <GtkFlowBoxChildAccessible>
           <GtkFontButton>
           <GtkFontChooserDialog>
           <GtkFontChooserWidget>
           <GtkFontSelection>
           <GtkFontSelectionDialog>
           <GtkFrame>
           <GtkFrameAccessible>
           <GtkGLArea>
           <GtkGesture>
           <GtkGestureDrag>
           <GtkGestureLongPress>
           <GtkGestureMultiPress>
           <GtkGesturePan>
           <GtkGestureRotate>
           <GtkGestureSingle>
           <GtkGestureStylus>
           <GtkGestureSwipe>
           <GtkGestureZoom>
           <GtkGradient>
           <GtkGrid>
           <GtkHBox>
           <GtkHButtonBox>
           <GtkHPaned>
           <GtkHSV>
           <GtkHScale>
           <GtkHScrollbar>
           <GtkHSeparator>
           <GtkHandleBox>
           <GtkHeaderBar>
           <GtkIMContext>
           <GtkIMContextSimple>
           <GtkIMMulticontext>
           <GtkIconFactory>
           <GtkIconInfo>
           <GtkIconSet>
           <GtkIconSource>
           <GtkIconTheme>
           <GtkIconView>
           <GtkIconViewAccessible>
           <GtkImage>
           <GtkImageAccessible>
           <GtkImageCellAccessible>
           <GtkImageMenuItem>
           <GtkInfoBar>
           <GtkInvisible>
           <GtkLabel>
           <GtkLabelAccessible>
           <GtkLayout>
           <GtkLevelBar>
           <GtkLevelBarAccessible>
           <GtkLinkButton>
           <GtkLinkButtonAccessible>
           <GtkListBox>
           <GtkListBoxAccessible>
           <GtkListBoxRow>
           <GtkListBoxRowAccessible>
           <GtkListStore>
           <GtkLockButton>
           <GtkLockButtonAccessible>
           <GtkMenu>
           <GtkMenuAccessible>
           <GtkMenuBar>
           <GtkMenuButton>
           <GtkMenuButtonAccessible>
           <GtkMenuItem>
           <GtkMenuItemAccessible>
           <GtkMenuShell>
           <GtkMenuShellAccessible>
           <GtkMenuToolButton>
           <GtkMessageDialog>
           <GtkMisc>
           <GtkModelButton>
           <GtkMountOperation>
           <GtkNativeDialog>
           <GtkNotebook>
           <GtkNotebookAccessible>
           <GtkNotebookPageAccessible>
           <GtkNumerableIcon>
           <GtkOffscreenWindow>
           <GtkOverlay>
           <GtkPadController>
           <GtkPageSetup>
           <GtkPaned>
           <GtkPanedAccessible>
           <GtkPaperSize>
           <GtkPlacesSidebar>
           <GtkPopover>
           <GtkPopoverAccessible>
           <GtkPopoverMenu>
           <GtkPrintContext>
           <GtkPrintOperation>
           <GtkPrintSettings>
           <GtkProgressBar>
           <GtkProgressBarAccessible>
           <GtkRadioAction>
           <GtkRadioButton>
           <GtkRadioButtonAccessible>
           <GtkRadioMenuItem>
           <GtkRadioMenuItemAccessible>
           <GtkRadioToolButton>
           <GtkRange>
           <GtkRangeAccessible>
           <GtkRcStyle>
           <GtkRecentAction>
           <GtkRecentChooserDialog>
           <GtkRecentChooserMenu>
           <GtkRecentChooserWidget>
           <GtkRecentFilter>
           <GtkRecentInfo>
           <GtkRecentManager>
           <GtkRendererCellAccessible>
           <GtkRequisition>
           <GtkRevealer>
           <GtkScale>
           <GtkScaleAccessible>
           <GtkScaleButton>
           <GtkScaleButtonAccessible>
           <GtkScrollbar>
           <GtkScrolledWindow>
           <GtkScrolledWindowAccessible>
           <GtkSearchBar>
           <GtkSearchEntry>
           <GtkSelectionData>
           <GtkSeparator>
           <GtkSeparatorMenuItem>
           <GtkSeparatorToolItem>
           <GtkSettings>
           <GtkShortcutLabel>
           <GtkShortcutsGroup>
           <GtkShortcutsSection>
           <GtkShortcutsShortcut>
           <GtkShortcutsWindow>
           <GtkSizeGroup>
           <GtkSpinButton>
           <GtkSpinButtonAccessible>
           <GtkSpinner>
           <GtkSpinnerAccessible>
           <GtkStack>
           <GtkStackAccessible>
           <GtkStackSidebar>
           <GtkStackSwitcher>
           <GtkStatusIcon>
           <GtkStatusbar>
           <GtkStatusbarAccessible>
           <GtkStyle>
           <GtkStyleContext>
           <GtkStyleProperties>
           <GtkSwitch>
           <GtkSwitchAccessible>
           <GtkSymbolicColor>
           <GtkTable>
           <GtkTargetEntry>
           <GtkTargetList>
           <GtkTearoffMenuItem>
           <GtkTextAttributes>
           <GtkTextBuffer>
           <GtkTextCellAccessible>
           <GtkTextChildAnchor>
           <GtkTextIter>
           <GtkTextMark>
           <GtkTextTag>
           <GtkTextTagTable>
           <GtkTextView>
           <GtkTextViewAccessible>
           <GtkThemingEngine>
           <GtkToggleAction>
           <GtkToggleButton>
           <GtkToggleButtonAccessible>
           <GtkToggleToolButton>
           <GtkToolButton>
           <GtkToolItem>
           <GtkToolItemGroup>
           <GtkToolPalette>
           <GtkToolbar>
           <GtkTooltip>
           <GtkToplevelAccessible>
           <GtkTreeIter>
           <GtkTreeModelFilter>
           <GtkTreeModelSort>
           <GtkTreePath>
           <GtkTreeRowReference>
           <GtkTreeSelection>
           <GtkTreeStore>
           <GtkTreeView>
           <GtkTreeViewAccessible>
           <GtkTreeViewColumn>
           <GtkUIManager>
           <GtkVBox>
           <GtkVButtonBox>
           <GtkVPaned>
           <GtkVScale>
           <GtkVScrollbar>
           <GtkVSeparator>
           <GtkViewport>
           <GtkVolumeButton>
           <GtkWidget>
           <GtkWidgetAccessible>
           <GtkWidgetPath>
           <GtkWindow>
           <GtkWindowAccessible>
           <GtkWindowGroup>
  ))

(eval-when (expand load compile)
  (load-typelib "Gtk" "3.0"))

;; Declaration for Gtk 3.0
(define <GtkAboutDialog>
  (gi-lookup-type "Gtk-AboutDialog"))

(define (GtkAboutDialog-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AboutDialog-new"))

(define (GtkAboutDialog-add-credit-section self section-name people)
"  ARGS: 
     section-name  - string, 
     people  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_credit_section" section-name people)))

(define (GtkAboutDialog-get-artists self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_artists")))

(define (GtkAboutDialog-get-authors self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_authors")))

(define (GtkAboutDialog-get-comments self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_comments")))

(define (GtkAboutDialog-get-copyright self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_copyright")))

(define (GtkAboutDialog-get-documenters self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_documenters")))

(define (GtkAboutDialog-get-license self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_license")))

(define (GtkAboutDialog-get-license-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_license_type")))

(define (GtkAboutDialog-get-logo self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_logo")))

(define (GtkAboutDialog-get-logo-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_logo_icon_name")))

(define (GtkAboutDialog-get-program-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_program_name")))

(define (GtkAboutDialog-get-translator-credits self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_translator_credits")))

(define (GtkAboutDialog-get-version self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_version")))

(define (GtkAboutDialog-get-website self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_website")))

(define (GtkAboutDialog-get-website-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_website_label")))

(define (GtkAboutDialog-get-wrap-license? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_wrap_license")))

(define (GtkAboutDialog-set-artists self artists)
"  ARGS: 
     artists  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_artists" artists)))

(define (GtkAboutDialog-set-authors self authors)
"  ARGS: 
     authors  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_authors" authors)))

(define (GtkAboutDialog-set-comments self comments)
"  ARGS: 
     comments  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_comments" comments)))

(define (GtkAboutDialog-set-copyright self copyright)
"  ARGS: 
     copyright  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_copyright" copyright)))

(define (GtkAboutDialog-set-documenters self documenters)
"  ARGS: 
     documenters  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_documenters" documenters)))

(define (GtkAboutDialog-set-license self license)
"  ARGS: 
     license  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_license" license)))

(define (GtkAboutDialog-set-license-type self license-type)
"  ARGS: 
     license-type  - exact integer of enum type License
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_license_type" license-type)))

(define (GtkAboutDialog-set-logo self logo)
"  ARGS: 
     logo  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_logo" logo)))

(define (GtkAboutDialog-set-logo-icon-name self icon-name)
"  ARGS: 
     icon-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_logo_icon_name" icon-name)))

(define (GtkAboutDialog-set-program-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_program_name" name)))

(define (GtkAboutDialog-set-translator-credits self translator-credits)
"  ARGS: 
     translator-credits  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_translator_credits" translator-credits)))

(define (GtkAboutDialog-set-version self version)
"  ARGS: 
     version  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_version" version)))

(define (GtkAboutDialog-set-website self website)
"  ARGS: 
     website  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_website" website)))

(define (GtkAboutDialog-set-website-label self website-label)
"  ARGS: 
     website-label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_website_label" website-label)))

(define (GtkAboutDialog-set-wrap-license self wrap-license)
"  ARGS: 
     wrap-license  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wrap_license" wrap-license)))

(define VISIBLE
  (gi-flag-value "Gtk-AccelFlags" "visible"))

(define LOCKED
  (gi-flag-value "Gtk-AccelFlags" "locked"))

(define MASK
  (gi-flag-value "Gtk-AccelFlags" "mask"))

(define <GtkAccelGroup>
  (gi-lookup-type "Gtk-AccelGroup"))

(define (GtkAccelGroup-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AccelGroup-new"))

(define (GtkAccelGroup-from-accel-closure closure)
"  ARGS: 
     closure  - struct Closure
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AccelGroup-from_accel_closure" closure))

(define (GtkAccelGroup-activate? self accel-quark acceleratable accel-key accel-mods)
"  ARGS: 
     accel-quark  - exact integer of size guint32, 
     acceleratable  - object Object, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate" accel-quark acceleratable accel-key accel-mods)))

(define (GtkAccelGroup-connect self accel-key accel-mods accel-flags closure)
"  ARGS: 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType, 
     accel-flags  - exact integer of flags type AccelFlags, 
     closure  - struct Closure
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect" accel-key accel-mods accel-flags closure)))

(define (GtkAccelGroup-connect-by-path self accel-path closure)
"  ARGS: 
     accel-path  - string, 
     closure  - struct Closure
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_by_path" accel-path closure)))

(define (GtkAccelGroup-disconnect? self closure)
"  ARGS: 
     closure  - struct Closure
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "disconnect" closure)))

(define (GtkAccelGroup-disconnect-key? self accel-key accel-mods)
"  ARGS: 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "disconnect_key" accel-key accel-mods)))

(define (GtkAccelGroup-find self find-func data)
"  ARGS: 
     find-func  - procedure of type AccelGroupFindFunc, 
     data  - #f for NULL or pointer
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "find" find-func data)))

(define (GtkAccelGroup-get-is-locked? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_is_locked")))

(define (GtkAccelGroup-get-modifier-mask self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_modifier_mask")))

(define (GtkAccelGroup-lock self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "lock")))

(define (GtkAccelGroup-query self accel-key accel-mods)
"  ARGS: 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType, 
     n-entries  - exact integer of size guint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "query" accel-key accel-mods)))

(define (GtkAccelGroup-unlock self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unlock")))

;; CALLBACK
(define accel-group-activate
  (gi-lookup-callback-info "Gtk-AccelGroupActivate"))
;; ARGS: 
;;   accel-group  - object AccelGroup, 
;;   acceleratable  - object Object, 
;;   keyval  - exact integer of size guint32, 
;;   modifier  - exact integer of flags type ModifierType
;; RETURN: gboolean
;; CALLBACK
(define accel-group-find-func
  (gi-lookup-callback-info "Gtk-AccelGroupFindFunc"))
;; ARGS: 
;;   key  - struct AccelKey, 
;;   closure  - struct Closure, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkAccelLabel>
  (gi-lookup-type "Gtk-AccelLabel"))

(define (GtkAccelLabel-new string)
"  ARGS: 
     string  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AccelLabel-new" string))

(define (GtkAccelLabel-get-accel self)
"  ARGS: 
     accelerator-key  - exact integer of size guint32[OUT], 
     accelerator-mods  - exact integer of flags type ModifierType[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_accel")))

(define (GtkAccelLabel-get-accel-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_widget")))

(define (GtkAccelLabel-get-accel-width self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_width")))

(define (GtkAccelLabel-refetch? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "refetch")))

(define (GtkAccelLabel-set-accel self accelerator-key accelerator-mods)
"  ARGS: 
     accelerator-key  - exact integer of size guint32, 
     accelerator-mods  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel" accelerator-key accelerator-mods)))

(define (GtkAccelLabel-set-accel-closure self accel-closure)
"  ARGS: 
     accel-closure  - struct Closure
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_closure" accel-closure)))

(define (GtkAccelLabel-set-accel-widget self accel-widget)
"  ARGS: 
     accel-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_widget" accel-widget)))

(define <GtkAccelMap>
  (gi-lookup-type "Gtk-AccelMap"))

(define (GtkAccelMap-add-entry accel-path accel-key accel-mods)
"  ARGS: 
     accel-path  - string, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-add_entry" accel-path accel-key accel-mods))

(define (GtkAccelMap-add-filter filter-pattern)
"  ARGS: 
     filter-pattern  - string
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-add_filter" filter-pattern))

(define (GtkAccelMap-change-entry? accel-path accel-key accel-mods replace)
"  ARGS: 
     accel-path  - string, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType, 
     replace  - boolean
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-AccelMap-change_entry" accel-path accel-key accel-mods replace))

(define (GtkAccelMap-foreach data foreach-func)
"  ARGS: 
     data  - #f for NULL or pointer, 
     foreach-func  - procedure of type AccelMapForeach
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-foreach" data foreach-func))

(define (GtkAccelMap-foreach-unfiltered data foreach-func)
"  ARGS: 
     data  - #f for NULL or pointer, 
     foreach-func  - procedure of type AccelMapForeach
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-foreach_unfiltered" data foreach-func))

(define (GtkAccelMap-get)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AccelMap-get"))

(define (GtkAccelMap-load file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-load" file-name))

(define (GtkAccelMap-load-fd fd)
"  ARGS: 
     fd  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-load_fd" fd))

(define (GtkAccelMap-load-scanner scanner)
"  ARGS: 
     scanner  - struct Scanner
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-load_scanner" scanner))

(define (GtkAccelMap-lock-path accel-path)
"  ARGS: 
     accel-path  - string
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-lock_path" accel-path))

(define (GtkAccelMap-lookup-entry? accel-path out-key)
"  ARGS: 
     accel-path  - string, 
   RETURN: gboolean
     key  - Unhandled argument type tag 16
"
  (gi-function-invoke "Gtk-AccelMap-lookup_entry" accel-path out-key))

(define (GtkAccelMap-save file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-save" file-name))

(define (GtkAccelMap-save-fd fd)
"  ARGS: 
     fd  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-save_fd" fd))

(define (GtkAccelMap-unlock-path accel-path)
"  ARGS: 
     accel-path  - string
   RETURN: void
"
  (gi-function-invoke "Gtk-AccelMap-unlock_path" accel-path))

;; CALLBACK
(define accel-map-foreach
  (gi-lookup-callback-info "Gtk-AccelMapForeach"))
;; ARGS: 
;;   data  - #f for NULL or pointer, 
;;   accel-path  - string, 
;;   accel-key  - exact integer of size guint32, 
;;   accel-mods  - exact integer of flags type ModifierType, 
;;   changed  - boolean
;; RETURN: void
(define <GtkAccessible>
  (gi-lookup-type "Gtk-Accessible"))

(define (GtkAccessible-connect-widget-destroyed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_widget_destroyed")))

(define (GtkAccessible-get-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget")))

(define (GtkAccessible-set-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_widget" widget)))

(define <GtkAction>
  (gi-lookup-type "Gtk-Action"))

(define (GtkAction-new name label tooltip stock-id)
"  ARGS: 
     name  - string, 
     label  - #f for NULL or string, 
     tooltip  - #f for NULL or string, 
     stock-id  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Action-new" name label tooltip stock-id))

(define (GtkAction-activate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "activate")))

(define (GtkAction-block-activate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "block_activate")))

(define (GtkAction-connect-accelerator self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_accelerator")))

(define (GtkAction-create-icon self icon-size)
"  ARGS: 
     icon-size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_icon" icon-size)))

(define (GtkAction-create-menu self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_menu")))

(define (GtkAction-create-menu-item self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_menu_item")))

(define (GtkAction-create-tool-item self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_tool_item")))

(define (GtkAction-disconnect-accelerator self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "disconnect_accelerator")))

(define (GtkAction-get-accel-closure self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_closure")))

(define (GtkAction-get-accel-path self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_path")))

(define (GtkAction-get-always-show-image? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_always_show_image")))

(define (GtkAction-get-gicon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_gicon")))

(define (GtkAction-get-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkAction-get-is-important? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_is_important")))

(define (GtkAction-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkAction-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkAction-get-proxies self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_proxies")))

(define (GtkAction-get-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_sensitive")))

(define (GtkAction-get-short-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_short_label")))

(define (GtkAction-get-stock-id self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_stock_id")))

(define (GtkAction-get-tooltip self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip")))

(define (GtkAction-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkAction-get-visible-horizontal? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_horizontal")))

(define (GtkAction-get-visible-vertical? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_vertical")))

(define (GtkAction-is-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_sensitive")))

(define (GtkAction-is-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_visible")))

(define (GtkAction-set-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_group" accel-group)))

(define (GtkAction-set-accel-path self accel-path)
"  ARGS: 
     accel-path  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_path" accel-path)))

(define (GtkAction-set-always-show-image self always-show)
"  ARGS: 
     always-show  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_always_show_image" always-show)))

(define (GtkAction-set-gicon self icon)
"  ARGS: 
     icon  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_gicon" icon)))

(define (GtkAction-set-icon-name self icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_name" icon-name)))

(define (GtkAction-set-is-important self is-important)
"  ARGS: 
     is-important  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_is_important" is-important)))

(define (GtkAction-set-label self label)
"  ARGS: 
     label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkAction-set-sensitive self sensitive)
"  ARGS: 
     sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sensitive" sensitive)))

(define (GtkAction-set-short-label self short-label)
"  ARGS: 
     short-label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_short_label" short-label)))

(define (GtkAction-set-stock-id self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_stock_id" stock-id)))

(define (GtkAction-set-tooltip self tooltip)
"  ARGS: 
     tooltip  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip" tooltip)))

(define (GtkAction-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define (GtkAction-set-visible-horizontal self visible-horizontal)
"  ARGS: 
     visible-horizontal  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_horizontal" visible-horizontal)))

(define (GtkAction-set-visible-vertical self visible-vertical)
"  ARGS: 
     visible-vertical  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_vertical" visible-vertical)))

(define (GtkAction-unblock-activate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unblock_activate")))

(define <GtkActionBar>
  (gi-lookup-type "Gtk-ActionBar"))

(define (GtkActionBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ActionBar-new"))

(define (GtkActionBar-get-center-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_center_widget")))

(define (GtkActionBar-pack-end self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_end" child)))

(define (GtkActionBar-pack-start self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_start" child)))

(define (GtkActionBar-set-center-widget self center-widget)
"  ARGS: 
     center-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_center_widget" center-widget)))

(define <GtkActionGroup>
  (gi-lookup-type "Gtk-ActionGroup"))

(define (GtkActionGroup-new name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ActionGroup-new" name))

(define (GtkActionGroup-add-action self action)
"  ARGS: 
     action  - object Action
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_action" action)))

(define (GtkActionGroup-add-action-with-accel self action accelerator)
"  ARGS: 
     action  - object Action, 
     accelerator  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_action_with_accel" action accelerator)))

(define (GtkActionGroup-get-accel-group self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_group")))

(define (GtkActionGroup-get-action self action-name)
"  ARGS: 
     action-name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action" action-name)))

(define (GtkActionGroup-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkActionGroup-get-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_sensitive")))

(define (GtkActionGroup-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkActionGroup-list-actions self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_actions")))

(define (GtkActionGroup-remove-action self action)
"  ARGS: 
     action  - object Action
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_action" action)))

(define (GtkActionGroup-set-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_group" accel-group)))

(define (GtkActionGroup-set-sensitive self sensitive)
"  ARGS: 
     sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sensitive" sensitive)))

(define (GtkActionGroup-set-translate-func self func data notify)
"  ARGS: 
     func  - procedure of type TranslateFunc, 
     data  - #f for NULL or pointer, 
     notify  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_translate_func" func data notify)))

(define (GtkActionGroup-set-translation-domain self domain)
"  ARGS: 
     domain  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_translation_domain" domain)))

(define (GtkActionGroup-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define (GtkActionGroup-translate-string self string)
"  ARGS: 
     string  - string
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "translate_string" string)))

(define <GtkAdjustment>
  (gi-lookup-type "Gtk-Adjustment"))

(define (GtkAdjustment-new value lower upper step-increment page-increment page-size)
"  ARGS: 
     value  - real number of size gdouble, 
     lower  - real number of size gdouble, 
     upper  - real number of size gdouble, 
     step-increment  - real number of size gdouble, 
     page-increment  - real number of size gdouble, 
     page-size  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Adjustment-new" value lower upper step-increment page-increment page-size))

(define (GtkAdjustment-changed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "changed")))

(define (GtkAdjustment-clamp-page self lower upper)
"  ARGS: 
     lower  - real number of size gdouble, 
     upper  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clamp_page" lower upper)))

(define (GtkAdjustment-configure self value lower upper step-increment page-increment page-size)
"  ARGS: 
     value  - real number of size gdouble, 
     lower  - real number of size gdouble, 
     upper  - real number of size gdouble, 
     step-increment  - real number of size gdouble, 
     page-increment  - real number of size gdouble, 
     page-size  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "configure" value lower upper step-increment page-increment page-size)))

(define (GtkAdjustment-get-lower self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_lower")))

(define (GtkAdjustment-get-minimum-increment self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_minimum_increment")))

(define (GtkAdjustment-get-page-increment self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_page_increment")))

(define (GtkAdjustment-get-page-size self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_page_size")))

(define (GtkAdjustment-get-step-increment self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_step_increment")))

(define (GtkAdjustment-get-upper self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_upper")))

(define (GtkAdjustment-get-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_value")))

(define (GtkAdjustment-set-lower self lower)
"  ARGS: 
     lower  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_lower" lower)))

(define (GtkAdjustment-set-page-increment self page-increment)
"  ARGS: 
     page-increment  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_increment" page-increment)))

(define (GtkAdjustment-set-page-size self page-size)
"  ARGS: 
     page-size  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_size" page-size)))

(define (GtkAdjustment-set-step-increment self step-increment)
"  ARGS: 
     step-increment  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_step_increment" step-increment)))

(define (GtkAdjustment-set-upper self upper)
"  ARGS: 
     upper  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_upper" upper)))

(define (GtkAdjustment-set-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" value)))

(define (GtkAdjustment-value-changed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "value_changed")))

(define ALIGN_FILL
  (gi-enum-value "Gtk-Align" "fill"))

(define ALIGN_START
  (gi-enum-value "Gtk-Align" "start"))

(define ALIGN_END
  (gi-enum-value "Gtk-Align" "end"))

(define ALIGN_CENTER
  (gi-enum-value "Gtk-Align" "center"))

(define ALIGN_BASELINE
  (gi-enum-value "Gtk-Align" "baseline"))

(define <GtkAlignment>
  (gi-lookup-type "Gtk-Alignment"))

(define (GtkAlignment-new xalign yalign xscale yscale)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat, 
     xscale  - real number of size gfloat, 
     yscale  - real number of size gfloat
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Alignment-new" xalign yalign xscale yscale))

(define (GtkAlignment-get-padding self)
"  ARGS: 
     padding-top  - exact integer of size guint32[OUT], 
     padding-bottom  - exact integer of size guint32[OUT], 
     padding-left  - exact integer of size guint32[OUT], 
     padding-right  - exact integer of size guint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_padding")))

(define (GtkAlignment-set self xalign yalign xscale yscale)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat, 
     xscale  - real number of size gfloat, 
     yscale  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" xalign yalign xscale yscale)))

(define (GtkAlignment-set-padding self padding-top padding-bottom padding-left padding-right)
"  ARGS: 
     padding-top  - exact integer of size guint32, 
     padding-bottom  - exact integer of size guint32, 
     padding-left  - exact integer of size guint32, 
     padding-right  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_padding" padding-top padding-bottom padding-left padding-right)))

(define <GtkAppChooserButton>
  (gi-lookup-type "Gtk-AppChooserButton"))

(define (GtkAppChooserButton-new content-type)
"  ARGS: 
     content-type  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AppChooserButton-new" content-type))

(define (GtkAppChooserButton-append-custom-item self name label icon)
"  ARGS: 
     name  - string, 
     label  - string, 
     icon  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_custom_item" name label icon)))

(define (GtkAppChooserButton-append-separator self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_separator")))

(define (GtkAppChooserButton-get-heading self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_heading")))

(define (GtkAppChooserButton-get-show-default-item? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_default_item")))

(define (GtkAppChooserButton-get-show-dialog-item? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_dialog_item")))

(define (GtkAppChooserButton-set-active-custom-item self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active_custom_item" name)))

(define (GtkAppChooserButton-set-heading self heading)
"  ARGS: 
     heading  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_heading" heading)))

(define (GtkAppChooserButton-set-show-default-item self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_default_item" setting)))

(define (GtkAppChooserButton-set-show-dialog-item self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_dialog_item" setting)))

(define <GtkAppChooserDialog>
  (gi-lookup-type "Gtk-AppChooserDialog"))

(define (GtkAppChooserDialog-new parent flags file)
"  ARGS: 
     parent  - object Window, 
     flags  - exact integer of flags type DialogFlags, 
     file  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AppChooserDialog-new" parent flags file))

(define (GtkAppChooserDialog-new-for-content-type parent flags content-type)
"  ARGS: 
     parent  - object Window, 
     flags  - exact integer of flags type DialogFlags, 
     content-type  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AppChooserDialog-new_for_content_type" parent flags content-type))

(define (GtkAppChooserDialog-get-heading self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_heading")))

(define (GtkAppChooserDialog-get-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget")))

(define (GtkAppChooserDialog-set-heading self heading)
"  ARGS: 
     heading  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_heading" heading)))

(define <GtkAppChooserWidget>
  (gi-lookup-type "Gtk-AppChooserWidget"))

(define (GtkAppChooserWidget-new content-type)
"  ARGS: 
     content-type  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AppChooserWidget-new" content-type))

(define (GtkAppChooserWidget-get-default-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_text")))

(define (GtkAppChooserWidget-get-show-all? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_all")))

(define (GtkAppChooserWidget-get-show-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_default")))

(define (GtkAppChooserWidget-get-show-fallback? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_fallback")))

(define (GtkAppChooserWidget-get-show-other? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_other")))

(define (GtkAppChooserWidget-get-show-recommended? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_recommended")))

(define (GtkAppChooserWidget-set-default-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_text" text)))

(define (GtkAppChooserWidget-set-show-all self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_all" setting)))

(define (GtkAppChooserWidget-set-show-default self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_default" setting)))

(define (GtkAppChooserWidget-set-show-fallback self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_fallback" setting)))

(define (GtkAppChooserWidget-set-show-other self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_other" setting)))

(define (GtkAppChooserWidget-set-show-recommended self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_recommended" setting)))

(define <GtkApplication>
  (gi-lookup-type "Gtk-Application"))

(define (GtkApplication-new application-id flags)
"  ARGS: 
     application-id  - #f for NULL or string, 
     flags  - exact integer of flags type ApplicationFlags
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Application-new" application-id flags))

(define (GtkApplication-add-accelerator self accelerator action-name parameter)
"  ARGS: 
     accelerator  - string, 
     action-name  - string, 
     parameter  - struct Variant
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_accelerator" accelerator action-name parameter)))

(define (GtkApplication-add-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_window" window)))

(define (GtkApplication-get-accels-for-action self detailed-action-name)
"  ARGS: 
     detailed-action-name  - string
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_accels_for_action" detailed-action-name)))

(define (GtkApplication-get-actions-for-accel self accel)
"  ARGS: 
     accel  - string
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_actions_for_accel" accel)))

(define (GtkApplication-get-active-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_active_window")))

(define (GtkApplication-get-app-menu self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_app_menu")))

(define (GtkApplication-get-menu-by-id self id)
"  ARGS: 
     id  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_menu_by_id" id)))

(define (GtkApplication-get-menubar self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_menubar")))

(define (GtkApplication-get-window-by-id self id)
"  ARGS: 
     id  - exact integer of size guint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_window_by_id" id)))

(define (GtkApplication-get-windows self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_windows")))

(define (GtkApplication-inhibit self window flags reason)
"  ARGS: 
     window  - object Window, 
     flags  - exact integer of flags type ApplicationInhibitFlags, 
     reason  - #f for NULL or string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "inhibit" window flags reason)))

(define (GtkApplication-is-inhibited? self flags)
"  ARGS: 
     flags  - exact integer of flags type ApplicationInhibitFlags
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_inhibited" flags)))

(define (GtkApplication-list-action-descriptions self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "list_action_descriptions")))

(define (GtkApplication-prefers-app-menu? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "prefers_app_menu")))

(define (GtkApplication-remove-accelerator self action-name parameter)
"  ARGS: 
     action-name  - string, 
     parameter  - struct Variant
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_accelerator" action-name parameter)))

(define (GtkApplication-remove-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_window" window)))

(define (GtkApplication-set-accels-for-action self detailed-action-name accels)
"  ARGS: 
     detailed-action-name  - string, 
     accels  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accels_for_action" detailed-action-name accels)))

(define (GtkApplication-set-app-menu self app-menu)
"  ARGS: 
     app-menu  - object MenuModel
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_app_menu" app-menu)))

(define (GtkApplication-set-menubar self menubar)
"  ARGS: 
     menubar  - object MenuModel
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_menubar" menubar)))

(define (GtkApplication-uninhibit self cookie)
"  ARGS: 
     cookie  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "uninhibit" cookie)))

(define LOGOUT
  (gi-flag-value "Gtk-ApplicationInhibitFlags" "logout"))

(define SWITCH
  (gi-flag-value "Gtk-ApplicationInhibitFlags" "switch"))

(define SUSPEND
  (gi-flag-value "Gtk-ApplicationInhibitFlags" "suspend"))

(define IDLE
  (gi-flag-value "Gtk-ApplicationInhibitFlags" "idle"))

(define <GtkApplicationWindow>
  (gi-lookup-type "Gtk-ApplicationWindow"))

(define (GtkApplicationWindow-new application)
"  ARGS: 
     application  - object Application
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ApplicationWindow-new" application))

(define (GtkApplicationWindow-get-help-overlay self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_help_overlay")))

(define (GtkApplicationWindow-get-id self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_id")))

(define (GtkApplicationWindow-get-show-menubar? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_menubar")))

(define (GtkApplicationWindow-set-help-overlay self help-overlay)
"  ARGS: 
     help-overlay  - object ShortcutsWindow
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_help_overlay" help-overlay)))

(define (GtkApplicationWindow-set-show-menubar self show-menubar)
"  ARGS: 
     show-menubar  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_menubar" show-menubar)))

(define <GtkArrow>
  (gi-lookup-type "Gtk-Arrow"))

(define (GtkArrow-new arrow-type shadow-type)
"  ARGS: 
     arrow-type  - exact integer of enum type ArrowType, 
     shadow-type  - exact integer of enum type ShadowType
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Arrow-new" arrow-type shadow-type))

(define (GtkArrow-set self arrow-type shadow-type)
"  ARGS: 
     arrow-type  - exact integer of enum type ArrowType, 
     shadow-type  - exact integer of enum type ShadowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" arrow-type shadow-type)))

(define <GtkArrowAccessible>
  (gi-lookup-type "Gtk-ArrowAccessible"))

(define ARROW_PLACEMENT_BOTH
  (gi-enum-value "Gtk-ArrowPlacement" "both"))

(define ARROW_PLACEMENT_START
  (gi-enum-value "Gtk-ArrowPlacement" "start"))

(define ARROW_PLACEMENT_END
  (gi-enum-value "Gtk-ArrowPlacement" "end"))

(define ARROW_TYPE_UP
  (gi-enum-value "Gtk-ArrowType" "up"))

(define ARROW_TYPE_DOWN
  (gi-enum-value "Gtk-ArrowType" "down"))

(define ARROW_TYPE_LEFT
  (gi-enum-value "Gtk-ArrowType" "left"))

(define ARROW_TYPE_RIGHT
  (gi-enum-value "Gtk-ArrowType" "right"))

(define ARROW_TYPE_NONE
  (gi-enum-value "Gtk-ArrowType" "none"))

(define <GtkAspectFrame>
  (gi-lookup-type "Gtk-AspectFrame"))

(define (GtkAspectFrame-new label xalign yalign ratio obey-child)
"  ARGS: 
     label  - #f for NULL or string, 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat, 
     ratio  - real number of size gfloat, 
     obey-child  - boolean
   RETURN: interface*
"
  (gi-function-invoke "Gtk-AspectFrame-new" label xalign yalign ratio obey-child))

(define (GtkAspectFrame-set self xalign yalign ratio obey-child)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat, 
     ratio  - real number of size gfloat, 
     obey-child  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" xalign yalign ratio obey-child)))

(define <GtkAssistant>
  (gi-lookup-type "Gtk-Assistant"))

(define (GtkAssistant-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Assistant-new"))

(define (GtkAssistant-add-action-widget self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_action_widget" child)))

(define (GtkAssistant-append-page self page)
"  ARGS: 
     page  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_page" page)))

(define (GtkAssistant-commit self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "commit")))

(define (GtkAssistant-get-current-page self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_current_page")))

(define (GtkAssistant-get-n-pages self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_pages")))

(define (GtkAssistant-get-nth-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_nth_page" page-num)))

(define (GtkAssistant-get-page-complete? self page)
"  ARGS: 
     page  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_page_complete" page)))

(define (GtkAssistant-get-page-has-padding? self page)
"  ARGS: 
     page  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_page_has_padding" page)))

(define (GtkAssistant-get-page-header-image self page)
"  ARGS: 
     page  - object Widget
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_page_header_image" page)))

(define (GtkAssistant-get-page-side-image self page)
"  ARGS: 
     page  - object Widget
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_page_side_image" page)))

(define (GtkAssistant-get-page-title self page)
"  ARGS: 
     page  - object Widget
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_page_title" page)))

(define (GtkAssistant-get-page-type self page)
"  ARGS: 
     page  - object Widget
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_page_type" page)))

(define (GtkAssistant-insert-page self page position)
"  ARGS: 
     page  - object Widget, 
     position  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_page" page position)))

(define (GtkAssistant-next-page self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "next_page")))

(define (GtkAssistant-prepend-page self page)
"  ARGS: 
     page  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "prepend_page" page)))

(define (GtkAssistant-previous-page self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "previous_page")))

(define (GtkAssistant-remove-action-widget self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_action_widget" child)))

(define (GtkAssistant-remove-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_page" page-num)))

(define (GtkAssistant-set-current-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_page" page-num)))

(define (GtkAssistant-set-forward-page-func self page-func data destroy)
"  ARGS: 
     page-func  - procedure of type AssistantPageFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_forward_page_func" page-func data destroy)))

(define (GtkAssistant-set-page-complete self page complete)
"  ARGS: 
     page  - object Widget, 
     complete  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_complete" page complete)))

(define (GtkAssistant-set-page-has-padding self page has-padding)
"  ARGS: 
     page  - object Widget, 
     has-padding  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_has_padding" page has-padding)))

(define (GtkAssistant-set-page-header-image self page pixbuf)
"  ARGS: 
     page  - object Widget, 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_header_image" page pixbuf)))

(define (GtkAssistant-set-page-side-image self page pixbuf)
"  ARGS: 
     page  - object Widget, 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_side_image" page pixbuf)))

(define (GtkAssistant-set-page-title self page title)
"  ARGS: 
     page  - object Widget, 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_title" page title)))

(define (GtkAssistant-set-page-type self page type)
"  ARGS: 
     page  - object Widget, 
     type  - exact integer of enum type AssistantPageType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_type" page type)))

(define (GtkAssistant-update-buttons-state self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "update_buttons_state")))

;; CALLBACK
(define assistant-page-func
  (gi-lookup-callback-info "Gtk-AssistantPageFunc"))
;; ARGS: 
;;   current-page  - exact integer of size gint32, 
;;   data  - #f for NULL or pointer
;; RETURN: gint32
(define ASSISTANT_PAGE_TYPE_CONTENT
  (gi-enum-value "Gtk-AssistantPageType" "content"))

(define ASSISTANT_PAGE_TYPE_INTRO
  (gi-enum-value "Gtk-AssistantPageType" "intro"))

(define ASSISTANT_PAGE_TYPE_CONFIRM
  (gi-enum-value "Gtk-AssistantPageType" "confirm"))

(define ASSISTANT_PAGE_TYPE_SUMMARY
  (gi-enum-value "Gtk-AssistantPageType" "summary"))

(define ASSISTANT_PAGE_TYPE_PROGRESS
  (gi-enum-value "Gtk-AssistantPageType" "progress"))

(define ASSISTANT_PAGE_TYPE_CUSTOM
  (gi-enum-value "Gtk-AssistantPageType" "custom"))

(define EXPAND
  (gi-flag-value "Gtk-AttachOptions" "expand"))

(define SHRINK
  (gi-flag-value "Gtk-AttachOptions" "shrink"))

(define FILL
  (gi-flag-value "Gtk-AttachOptions" "fill"))

(define GTK_BINARY_AGE
  (gi-constant-value "Gtk-BINARY_AGE"))

(define BASELINE_POSITION_TOP
  (gi-enum-value "Gtk-BaselinePosition" "top"))

(define BASELINE_POSITION_CENTER
  (gi-enum-value "Gtk-BaselinePosition" "center"))

(define BASELINE_POSITION_BOTTOM
  (gi-enum-value "Gtk-BaselinePosition" "bottom"))

(define <GtkBin>
  (gi-lookup-type "Gtk-Bin"))

(define (GtkBin-get-child self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child")))

(define <GtkBooleanCellAccessible>
  (gi-lookup-type "Gtk-BooleanCellAccessible"))

(define <GtkBorder>
  (gi-lookup-type "Gtk-Border"))

(define (GtkBorder-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Border-new"))

(define (GtkBorder-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkBorder-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define BORDER_STYLE_NONE
  (gi-enum-value "Gtk-BorderStyle" "none"))

(define BORDER_STYLE_SOLID
  (gi-enum-value "Gtk-BorderStyle" "solid"))

(define BORDER_STYLE_INSET
  (gi-enum-value "Gtk-BorderStyle" "inset"))

(define BORDER_STYLE_OUTSET
  (gi-enum-value "Gtk-BorderStyle" "outset"))

(define BORDER_STYLE_HIDDEN
  (gi-enum-value "Gtk-BorderStyle" "hidden"))

(define BORDER_STYLE_DOTTED
  (gi-enum-value "Gtk-BorderStyle" "dotted"))

(define BORDER_STYLE_DASHED
  (gi-enum-value "Gtk-BorderStyle" "dashed"))

(define BORDER_STYLE_DOUBLE
  (gi-enum-value "Gtk-BorderStyle" "double"))

(define BORDER_STYLE_GROOVE
  (gi-enum-value "Gtk-BorderStyle" "groove"))

(define BORDER_STYLE_RIDGE
  (gi-enum-value "Gtk-BorderStyle" "ridge"))

(define <GtkBox>
  (gi-lookup-type "Gtk-Box"))

(define (GtkBox-new orientation spacing)
"  ARGS: 
     orientation  - exact integer of enum type Orientation, 
     spacing  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Box-new" orientation spacing))

(define (GtkBox-get-baseline-position self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_baseline_position")))

(define (GtkBox-get-center-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_center_widget")))

(define (GtkBox-get-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_homogeneous")))

(define (GtkBox-get-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_spacing")))

(define (GtkBox-pack-end self child expand fill padding)
"  ARGS: 
     child  - object Widget, 
     expand  - boolean, 
     fill  - boolean, 
     padding  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_end" child expand fill padding)))

(define (GtkBox-pack-start self child expand fill padding)
"  ARGS: 
     child  - object Widget, 
     expand  - boolean, 
     fill  - boolean, 
     padding  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_start" child expand fill padding)))

(define (GtkBox-query-child-packing self child)
"  ARGS: 
     child  - object Widget, 
     expand  - boolean[OUT], 
     fill  - boolean[OUT], 
     padding  - exact integer of size guint32[OUT], 
     pack-type  - exact integer of enum type PackType[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "query_child_packing" child)))

(define (GtkBox-reorder-child self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reorder_child" child position)))

(define (GtkBox-set-baseline-position self position)
"  ARGS: 
     position  - exact integer of enum type BaselinePosition
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_baseline_position" position)))

(define (GtkBox-set-center-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_center_widget" widget)))

(define (GtkBox-set-child-packing self child expand fill padding pack-type)
"  ARGS: 
     child  - object Widget, 
     expand  - boolean, 
     fill  - boolean, 
     padding  - exact integer of size guint32, 
     pack-type  - exact integer of enum type PackType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_child_packing" child expand fill padding pack-type)))

(define (GtkBox-set-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_homogeneous" homogeneous)))

(define (GtkBox-set-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_spacing" spacing)))

(define <GtkBuilder>
  (gi-lookup-type "Gtk-Builder"))

(define (GtkBuilder-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Builder-new"))

(define (GtkBuilder-new-from-file filename)
"  ARGS: 
     filename  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Builder-new_from_file" filename))

(define (GtkBuilder-new-from-resource resource-path)
"  ARGS: 
     resource-path  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Builder-new_from_resource" resource-path))

(define (GtkBuilder-new-from-string string length)
"  ARGS: 
     string  - string, 
     length  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Builder-new_from_string" string length))

(define (GtkBuilder-add-callback-symbol self callback-name callback-symbol)
"  ARGS: 
     callback-name  - string, 
     callback-symbol  - procedure of type Callback
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_callback_symbol" callback-name callback-symbol)))

(define (GtkBuilder-add-from-file self filename)
"  ARGS: 
     filename  - string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_from_file" filename)))

(define (GtkBuilder-add-from-resource self resource-path)
"  ARGS: 
     resource-path  - string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_from_resource" resource-path)))

(define (GtkBuilder-add-from-string self buffer length)
"  ARGS: 
     buffer  - string, 
     length  - exact integer of size guint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_from_string" buffer length)))

(define (GtkBuilder-add-objects-from-file self filename object-ids)
"  ARGS: 
     filename  - string, 
     object-ids  - Unhandled argument type tag 15
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_objects_from_file" filename object-ids)))

(define (GtkBuilder-add-objects-from-resource self resource-path object-ids)
"  ARGS: 
     resource-path  - string, 
     object-ids  - Unhandled argument type tag 15
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_objects_from_resource" resource-path object-ids)))

(define (GtkBuilder-add-objects-from-string self buffer length object-ids)
"  ARGS: 
     buffer  - string, 
     length  - exact integer of size guint32, 
     object-ids  - Unhandled argument type tag 15
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_objects_from_string" buffer length object-ids)))

(define (GtkBuilder-connect-signals self user-data)
"  ARGS: 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_signals" user-data)))

(define (GtkBuilder-connect-signals-full self func user-data)
"  ARGS: 
     func  - procedure of type BuilderConnectFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_signals_full" func user-data)))

(define (GtkBuilder-expose-object self name object)
"  ARGS: 
     name  - string, 
     object  - object Object
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "expose_object" name object)))

(define (GtkBuilder-extend-with-template self widget template-type buffer length)
"  ARGS: 
     widget  - object Widget, 
     template-type  - <GType>, 
     buffer  - string, 
     length  - exact integer of size guint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "extend_with_template" widget template-type buffer length)))

(define (GtkBuilder-get-application self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_application")))

(define (GtkBuilder-get-object self name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_object" name)))

(define (GtkBuilder-get-objects self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_objects")))

(define (GtkBuilder-get-translation-domain self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_translation_domain")))

(define (GtkBuilder-get-type-from-name self type-name)
"  ARGS: 
     type-name  - string
   RETURN: GType
"
  (gi-method-send self 
     (gi-method-prepare "get_type_from_name" type-name)))

(define (GtkBuilder-set-application self application)
"  ARGS: 
     application  - object Application
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_application" application)))

(define (GtkBuilder-set-translation-domain self domain)
"  ARGS: 
     domain  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_translation_domain" domain)))

(define (GtkBuilder-value-from-string? self pspec string out-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     string  - string, 
   RETURN: gboolean
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "value_from_string" pspec string out-value)))

(define (GtkBuilder-value-from-string-type? self type string out-value)
"  ARGS: 
     type  - <GType>, 
     string  - string, 
   RETURN: gboolean
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "value_from_string_type" type string out-value)))

;; CALLBACK
(define builder-connect-func
  (gi-lookup-callback-info "Gtk-BuilderConnectFunc"))
;; ARGS: 
;;   builder  - object Builder, 
;;   object  - object Object, 
;;   signal-name  - string, 
;;   handler-name  - string, 
;;   connect-object  - object Object, 
;;   flags  - exact integer of flags type ConnectFlags, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define BUILDER_ERROR_INVALID_TYPE_FUNCTION
  (gi-enum-value "Gtk-BuilderError" "invalid_type_function"))

(define BUILDER_ERROR_UNHANDLED_TAG
  (gi-enum-value "Gtk-BuilderError" "unhandled_tag"))

(define BUILDER_ERROR_MISSING_ATTRIBUTE
  (gi-enum-value "Gtk-BuilderError" "missing_attribute"))

(define BUILDER_ERROR_INVALID_ATTRIBUTE
  (gi-enum-value "Gtk-BuilderError" "invalid_attribute"))

(define BUILDER_ERROR_INVALID_TAG
  (gi-enum-value "Gtk-BuilderError" "invalid_tag"))

(define BUILDER_ERROR_MISSING_PROPERTY_VALUE
  (gi-enum-value "Gtk-BuilderError" "missing_property_value"))

(define BUILDER_ERROR_INVALID_VALUE
  (gi-enum-value "Gtk-BuilderError" "invalid_value"))

(define BUILDER_ERROR_VERSION_MISMATCH
  (gi-enum-value "Gtk-BuilderError" "version_mismatch"))

(define BUILDER_ERROR_DUPLICATE_ID
  (gi-enum-value "Gtk-BuilderError" "duplicate_id"))

(define BUILDER_ERROR_OBJECT_TYPE_REFUSED
  (gi-enum-value "Gtk-BuilderError" "object_type_refused"))

(define BUILDER_ERROR_TEMPLATE_MISMATCH
  (gi-enum-value "Gtk-BuilderError" "template_mismatch"))

(define BUILDER_ERROR_INVALID_PROPERTY
  (gi-enum-value "Gtk-BuilderError" "invalid_property"))

(define BUILDER_ERROR_INVALID_SIGNAL
  (gi-enum-value "Gtk-BuilderError" "invalid_signal"))

(define BUILDER_ERROR_INVALID_ID
  (gi-enum-value "Gtk-BuilderError" "invalid_id"))

(define <GtkButton>
  (gi-lookup-type "Gtk-Button"))

(define (GtkButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Button-new"))

(define (GtkButton-new-from-icon-name icon-name size)
"  ARGS: 
     icon-name  - #f for NULL or string, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Button-new_from_icon_name" icon-name size))

(define (GtkButton-new-from-stock stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Button-new_from_stock" stock-id))

(define (GtkButton-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Button-new_with_label" label))

(define (GtkButton-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Button-new_with_mnemonic" label))

(define (GtkButton-clicked self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clicked")))

(define (GtkButton-enter self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "enter")))

(define (GtkButton-get-alignment self)
"  ARGS: 
     xalign  - real number of size gfloat[OUT], 
     yalign  - real number of size gfloat[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_alignment")))

(define (GtkButton-get-always-show-image? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_always_show_image")))

(define (GtkButton-get-event-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_event_window")))

(define (GtkButton-get-focus-on-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_on_click")))

(define (GtkButton-get-image self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_image")))

(define (GtkButton-get-image-position self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_image_position")))

(define (GtkButton-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkButton-get-relief self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_relief")))

(define (GtkButton-get-use-stock? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_stock")))

(define (GtkButton-get-use-underline? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_underline")))

(define (GtkButton-leave self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "leave")))

(define (GtkButton-pressed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pressed")))

(define (GtkButton-released self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "released")))

(define (GtkButton-set-alignment self xalign yalign)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alignment" xalign yalign)))

(define (GtkButton-set-always-show-image self always-show)
"  ARGS: 
     always-show  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_always_show_image" always-show)))

(define (GtkButton-set-focus-on-click self focus-on-click)
"  ARGS: 
     focus-on-click  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_on_click" focus-on-click)))

(define (GtkButton-set-image self image)
"  ARGS: 
     image  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_image" image)))

(define (GtkButton-set-image-position self position)
"  ARGS: 
     position  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_image_position" position)))

(define (GtkButton-set-label self label)
"  ARGS: 
     label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkButton-set-relief self relief)
"  ARGS: 
     relief  - exact integer of enum type ReliefStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_relief" relief)))

(define (GtkButton-set-use-stock self use-stock)
"  ARGS: 
     use-stock  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_stock" use-stock)))

(define (GtkButton-set-use-underline self use-underline)
"  ARGS: 
     use-underline  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_underline" use-underline)))

(define <GtkButtonAccessible>
  (gi-lookup-type "Gtk-ButtonAccessible"))

(define <GtkButtonBox>
  (gi-lookup-type "Gtk-ButtonBox"))

(define (GtkButtonBox-new orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ButtonBox-new" orientation))

(define (GtkButtonBox-get-child-non-homogeneous? self child)
"  ARGS: 
     child  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_child_non_homogeneous" child)))

(define (GtkButtonBox-get-child-secondary? self child)
"  ARGS: 
     child  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_child_secondary" child)))

(define (GtkButtonBox-get-layout self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_layout")))

(define (GtkButtonBox-set-child-non-homogeneous self child non-homogeneous)
"  ARGS: 
     child  - object Widget, 
     non-homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_child_non_homogeneous" child non-homogeneous)))

(define (GtkButtonBox-set-child-secondary self child is-secondary)
"  ARGS: 
     child  - object Widget, 
     is-secondary  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_child_secondary" child is-secondary)))

(define (GtkButtonBox-set-layout self layout-style)
"  ARGS: 
     layout-style  - exact integer of enum type ButtonBoxStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_layout" layout-style)))

(define BUTTON_BOX_STYLE_SPREAD
  (gi-enum-value "Gtk-ButtonBoxStyle" "spread"))

(define BUTTON_BOX_STYLE_EDGE
  (gi-enum-value "Gtk-ButtonBoxStyle" "edge"))

(define BUTTON_BOX_STYLE_START
  (gi-enum-value "Gtk-ButtonBoxStyle" "start"))

(define BUTTON_BOX_STYLE_END
  (gi-enum-value "Gtk-ButtonBoxStyle" "end"))

(define BUTTON_BOX_STYLE_CENTER
  (gi-enum-value "Gtk-ButtonBoxStyle" "center"))

(define BUTTON_BOX_STYLE_EXPAND
  (gi-enum-value "Gtk-ButtonBoxStyle" "expand"))

(define BUTTON_ROLE_NORMAL
  (gi-enum-value "Gtk-ButtonRole" "normal"))

(define BUTTON_ROLE_CHECK
  (gi-enum-value "Gtk-ButtonRole" "check"))

(define BUTTON_ROLE_RADIO
  (gi-enum-value "Gtk-ButtonRole" "radio"))

(define BUTTONS_TYPE_NONE
  (gi-enum-value "Gtk-ButtonsType" "none"))

(define BUTTONS_TYPE_OK
  (gi-enum-value "Gtk-ButtonsType" "ok"))

(define BUTTONS_TYPE_CLOSE
  (gi-enum-value "Gtk-ButtonsType" "close"))

(define BUTTONS_TYPE_CANCEL
  (gi-enum-value "Gtk-ButtonsType" "cancel"))

(define BUTTONS_TYPE_YES_NO
  (gi-enum-value "Gtk-ButtonsType" "yes_no"))

(define BUTTONS_TYPE_OK_CANCEL
  (gi-enum-value "Gtk-ButtonsType" "ok_cancel"))

(define <GtkCalendar>
  (gi-lookup-type "Gtk-Calendar"))

(define (GtkCalendar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Calendar-new"))

(define (GtkCalendar-clear-marks self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear_marks")))

(define (GtkCalendar-get-date self)
"  ARGS: 
     year  - exact integer of size guint32[OUT], 
     month  - exact integer of size guint32[OUT], 
     day  - exact integer of size guint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_date")))

(define (GtkCalendar-get-day-is-marked? self day)
"  ARGS: 
     day  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_day_is_marked" day)))

(define (GtkCalendar-get-detail-height-rows self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_detail_height_rows")))

(define (GtkCalendar-get-detail-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_detail_width_chars")))

(define (GtkCalendar-get-display-options self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_display_options")))

(define (GtkCalendar-mark-day self day)
"  ARGS: 
     day  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "mark_day" day)))

(define (GtkCalendar-select-day self day)
"  ARGS: 
     day  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_day" day)))

(define (GtkCalendar-select-month self month year)
"  ARGS: 
     month  - exact integer of size guint32, 
     year  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_month" month year)))

(define (GtkCalendar-set-detail-func self func data destroy)
"  ARGS: 
     func  - procedure of type CalendarDetailFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_detail_func" func data destroy)))

(define (GtkCalendar-set-detail-height-rows self rows)
"  ARGS: 
     rows  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_detail_height_rows" rows)))

(define (GtkCalendar-set-detail-width-chars self chars)
"  ARGS: 
     chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_detail_width_chars" chars)))

(define (GtkCalendar-set-display-options self flags)
"  ARGS: 
     flags  - exact integer of flags type CalendarDisplayOptions
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_display_options" flags)))

(define (GtkCalendar-unmark-day self day)
"  ARGS: 
     day  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unmark_day" day)))

;; CALLBACK
(define calendar-detail-func
  (gi-lookup-callback-info "Gtk-CalendarDetailFunc"))
;; ARGS: 
;;   calendar  - object Calendar, 
;;   year  - exact integer of size guint32, 
;;   month  - exact integer of size guint32, 
;;   day  - exact integer of size guint32, 
;;   user-data  - #f for NULL or pointer
;; RETURN: utf8*
(define SHOW_HEADING
  (gi-flag-value "Gtk-CalendarDisplayOptions" "show_heading"))

(define SHOW_DAY_NAMES
  (gi-flag-value "Gtk-CalendarDisplayOptions" "show_day_names"))

(define NO_MONTH_CHANGE
  (gi-flag-value "Gtk-CalendarDisplayOptions" "no_month_change"))

(define SHOW_WEEK_NUMBERS
  (gi-flag-value "Gtk-CalendarDisplayOptions" "show_week_numbers"))

(define SHOW_DETAILS
  (gi-flag-value "Gtk-CalendarDisplayOptions" "show_details"))

;; CALLBACK
(define callback
  (gi-lookup-callback-info "Gtk-Callback"))
;; ARGS: 
;;   widget  - object Widget, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define <GtkCellAccessible>
  (gi-lookup-type "Gtk-CellAccessible"))

;; CALLBACK
(define cell-alloc-callback
  (gi-lookup-callback-info "Gtk-CellAllocCallback"))
;; ARGS: 
;;   renderer  - object CellRenderer, 
;;   cell-area  - struct Rectangle, 
;;   cell-background  - struct Rectangle, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkCellArea>
  (gi-lookup-type "Gtk-CellArea"))

(define (GtkCellArea-activate? self context widget cell-area flags edit-only)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState, 
     edit-only  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate" context widget cell-area flags edit-only)))

(define (GtkCellArea-activate-cell? self widget renderer event cell-area flags)
"  ARGS: 
     widget  - object Widget, 
     renderer  - object CellRenderer, 
     event  - union Event, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate_cell" widget renderer event cell-area flags)))

(define (GtkCellArea-add self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add" renderer)))

(define (GtkCellArea-add-focus-sibling self renderer sibling)
"  ARGS: 
     renderer  - object CellRenderer, 
     sibling  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_focus_sibling" renderer sibling)))

(define (GtkCellArea-apply-attributes self tree-model iter is-expander is-expanded)
"  ARGS: 
     tree-model  - Unhandled argument type tag 16, 
     iter  - struct TreeIter, 
     is-expander  - boolean, 
     is-expanded  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "apply_attributes" tree-model iter is-expander is-expanded)))

(define (GtkCellArea-attribute-connect self renderer attribute column)
"  ARGS: 
     renderer  - object CellRenderer, 
     attribute  - string, 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attribute_connect" renderer attribute column)))

(define (GtkCellArea-attribute-disconnect self renderer attribute)
"  ARGS: 
     renderer  - object CellRenderer, 
     attribute  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attribute_disconnect" renderer attribute)))

(define (GtkCellArea-attribute-get-column self renderer attribute)
"  ARGS: 
     renderer  - object CellRenderer, 
     attribute  - string
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "attribute_get_column" renderer attribute)))

(define (GtkCellArea-cell-get-property self renderer property-name value)
"  ARGS: 
     renderer  - object CellRenderer, 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cell_get_property" renderer property-name value)))

(define (GtkCellArea-cell-set-property self renderer property-name value)
"  ARGS: 
     renderer  - object CellRenderer, 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cell_set_property" renderer property-name value)))

(define (GtkCellArea-copy-context self context)
"  ARGS: 
     context  - object CellAreaContext
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy_context" context)))

(define (GtkCellArea-create-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_context")))

(define (GtkCellArea-event self context widget event cell-area flags)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     event  - union Event, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "event" context widget event cell-area flags)))

(define (GtkCellArea-focus? self direction)
"  ARGS: 
     direction  - exact integer of enum type DirectionType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "focus" direction)))

(define (GtkCellArea-foreach self callback callback-data)
"  ARGS: 
     callback  - procedure of type CellCallback, 
     callback-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "foreach" callback callback-data)))

(define (GtkCellArea-foreach-alloc self context widget cell-area background-area callback callback-data)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     cell-area  - struct Rectangle, 
     background-area  - struct Rectangle, 
     callback  - procedure of type CellAllocCallback, 
     callback-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "foreach_alloc" context widget cell-area background-area callback callback-data)))

(define (GtkCellArea-get-cell-allocation self context widget renderer cell-area out-allocation)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     renderer  - object CellRenderer, 
     cell-area  - struct Rectangle, 
   RETURN: void
     allocation  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_cell_allocation" context widget renderer cell-area out-allocation)))

(define (GtkCellArea-get-cell-at-position self context widget cell-area x y out-alloc-area)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     cell-area  - struct Rectangle, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
   RETURN: interface*
     alloc-area  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_cell_at_position" context widget cell-area x y out-alloc-area)))

(define (GtkCellArea-get-current-path-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_current_path_string")))

(define (GtkCellArea-get-edit-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_edit_widget")))

(define (GtkCellArea-get-edited-cell self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_edited_cell")))

(define (GtkCellArea-get-focus-cell self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_cell")))

(define (GtkCellArea-get-focus-from-sibling self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_from_sibling" renderer)))

(define (GtkCellArea-get-focus-siblings self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_siblings" renderer)))

(define (GtkCellArea-get-preferred-height self context widget)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height" context widget)))

(define (GtkCellArea-get-preferred-height-for-width self context widget width)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     width  - exact integer of size gint32, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height_for_width" context widget width)))

(define (GtkCellArea-get-preferred-width self context widget)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width" context widget)))

(define (GtkCellArea-get-preferred-width-for-height self context widget height)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     height  - exact integer of size gint32, 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width_for_height" context widget height)))

(define (GtkCellArea-get-request-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_request_mode")))

(define (GtkCellArea-has-renderer? self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_renderer" renderer)))

(define (GtkCellArea-inner-cell-area self widget cell-area out-inner-area)
"  ARGS: 
     widget  - object Widget, 
     cell-area  - struct Rectangle, 
   RETURN: void
     inner-area  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "inner_cell_area" widget cell-area out-inner-area)))

(define (GtkCellArea-is-activatable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_activatable")))

(define (GtkCellArea-is-focus-sibling? self renderer sibling)
"  ARGS: 
     renderer  - object CellRenderer, 
     sibling  - object CellRenderer
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_focus_sibling" renderer sibling)))

(define (GtkCellArea-remove self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" renderer)))

(define (GtkCellArea-remove-focus-sibling self renderer sibling)
"  ARGS: 
     renderer  - object CellRenderer, 
     sibling  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_focus_sibling" renderer sibling)))

(define (GtkCellArea-render self context widget cr background-area cell-area flags paint-focus)
"  ARGS: 
     context  - object CellAreaContext, 
     widget  - object Widget, 
     cr  - struct Context, 
     background-area  - struct Rectangle, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState, 
     paint-focus  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "render" context widget cr background-area cell-area flags paint-focus)))

(define (GtkCellArea-request-renderer self renderer orientation widget for-size)
"  ARGS: 
     renderer  - object CellRenderer, 
     orientation  - exact integer of enum type Orientation, 
     widget  - object Widget, 
     for-size  - exact integer of size gint32, 
     minimum-size  - exact integer of size gint32[OUT], 
     natural-size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_renderer" renderer orientation widget for-size)))

(define (GtkCellArea-set-focus-cell self renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_cell" renderer)))

(define (GtkCellArea-stop-editing self canceled)
"  ARGS: 
     canceled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "stop_editing" canceled)))

(define <GtkCellAreaBox>
  (gi-lookup-type "Gtk-CellAreaBox"))

(define (GtkCellAreaBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellAreaBox-new"))

(define (GtkCellAreaBox-get-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_spacing")))

(define (GtkCellAreaBox-pack-end self renderer expand align fixed)
"  ARGS: 
     renderer  - object CellRenderer, 
     expand  - boolean, 
     align  - boolean, 
     fixed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_end" renderer expand align fixed)))

(define (GtkCellAreaBox-pack-start self renderer expand align fixed)
"  ARGS: 
     renderer  - object CellRenderer, 
     expand  - boolean, 
     align  - boolean, 
     fixed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_start" renderer expand align fixed)))

(define (GtkCellAreaBox-set-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_spacing" spacing)))

(define <GtkCellAreaContext>
  (gi-lookup-type "Gtk-CellAreaContext"))

(define (GtkCellAreaContext-allocate self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "allocate" width height)))

(define (GtkCellAreaContext-get-allocation self)
"  ARGS: 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_allocation")))

(define (GtkCellAreaContext-get-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_area")))

(define (GtkCellAreaContext-get-preferred-height self)
"  ARGS: 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height")))

(define (GtkCellAreaContext-get-preferred-height-for-width self width)
"  ARGS: 
     width  - exact integer of size gint32, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height_for_width" width)))

(define (GtkCellAreaContext-get-preferred-width self)
"  ARGS: 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width")))

(define (GtkCellAreaContext-get-preferred-width-for-height self height)
"  ARGS: 
     height  - exact integer of size gint32, 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width_for_height" height)))

(define (GtkCellAreaContext-push-preferred-height self minimum-height natural-height)
"  ARGS: 
     minimum-height  - exact integer of size gint32, 
     natural-height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "push_preferred_height" minimum-height natural-height)))

(define (GtkCellAreaContext-push-preferred-width self minimum-width natural-width)
"  ARGS: 
     minimum-width  - exact integer of size gint32, 
     natural-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "push_preferred_width" minimum-width natural-width)))

(define (GtkCellAreaContext-reset self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset")))

;; CALLBACK
(define cell-callback
  (gi-lookup-callback-info "Gtk-CellCallback"))
;; ARGS: 
;;   renderer  - object CellRenderer, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define cell-layout-data-func
  (gi-lookup-callback-info "Gtk-CellLayoutDataFunc"))
;; ARGS: 
;;   cell-layout  - Unhandled argument type tag 16, 
;;   cell  - object CellRenderer, 
;;   tree-model  - Unhandled argument type tag 16, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define <GtkCellRenderer>
  (gi-lookup-type "Gtk-CellRenderer"))

(define (GtkCellRenderer-activate? self event widget path background-area cell-area flags)
"  ARGS: 
     event  - union Event, 
     widget  - object Widget, 
     path  - string, 
     background-area  - struct Rectangle, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate" event widget path background-area cell-area flags)))

(define (GtkCellRenderer-get-aligned-area self widget flags cell-area out-aligned-area)
"  ARGS: 
     widget  - object Widget, 
     flags  - exact integer of flags type CellRendererState, 
     cell-area  - struct Rectangle, 
   RETURN: void
     aligned-area  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_aligned_area" widget flags cell-area out-aligned-area)))

(define (GtkCellRenderer-get-alignment self)
"  ARGS: 
     xalign  - real number of size gfloat[OUT], 
     yalign  - real number of size gfloat[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_alignment")))

(define (GtkCellRenderer-get-fixed-size self)
"  ARGS: 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_fixed_size")))

(define (GtkCellRenderer-get-padding self)
"  ARGS: 
     xpad  - exact integer of size gint32[OUT], 
     ypad  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_padding")))

(define (GtkCellRenderer-get-preferred-height self widget)
"  ARGS: 
     widget  - object Widget, 
     minimum-size  - exact integer of size gint32[OUT], 
     natural-size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height" widget)))

(define (GtkCellRenderer-get-preferred-height-for-width self widget width)
"  ARGS: 
     widget  - object Widget, 
     width  - exact integer of size gint32, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height_for_width" widget width)))

(define (GtkCellRenderer-get-preferred-size self widget out-minimum-size out-natural-size)
"  ARGS: 
     widget  - object Widget, 
   RETURN: void
     minimum-size  - Unhandled argument type tag 16, 
     natural-size  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_size" widget out-minimum-size out-natural-size)))

(define (GtkCellRenderer-get-preferred-width self widget)
"  ARGS: 
     widget  - object Widget, 
     minimum-size  - exact integer of size gint32[OUT], 
     natural-size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width" widget)))

(define (GtkCellRenderer-get-preferred-width-for-height self widget height)
"  ARGS: 
     widget  - object Widget, 
     height  - exact integer of size gint32, 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width_for_height" widget height)))

(define (GtkCellRenderer-get-request-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_request_mode")))

(define (GtkCellRenderer-get-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_sensitive")))

(define (GtkCellRenderer-get-size self widget cell-area)
"  ARGS: 
     widget  - object Widget, 
     cell-area  - struct Rectangle, 
     x-offset  - exact integer of size gint32[OUT], 
     y-offset  - exact integer of size gint32[OUT], 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_size" widget cell-area)))

(define (GtkCellRenderer-get-state self widget cell-state)
"  ARGS: 
     widget  - object Widget, 
     cell-state  - exact integer of flags type CellRendererState
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state" widget cell-state)))

(define (GtkCellRenderer-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkCellRenderer-is-activatable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_activatable")))

(define (GtkCellRenderer-render self cr widget background-area cell-area flags)
"  ARGS: 
     cr  - struct Context, 
     widget  - object Widget, 
     background-area  - struct Rectangle, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "render" cr widget background-area cell-area flags)))

(define (GtkCellRenderer-set-alignment self xalign yalign)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alignment" xalign yalign)))

(define (GtkCellRenderer-set-fixed-size self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fixed_size" width height)))

(define (GtkCellRenderer-set-padding self xpad ypad)
"  ARGS: 
     xpad  - exact integer of size gint32, 
     ypad  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_padding" xpad ypad)))

(define (GtkCellRenderer-set-sensitive self sensitive)
"  ARGS: 
     sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sensitive" sensitive)))

(define (GtkCellRenderer-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define (GtkCellRenderer-start-editing self event widget path background-area cell-area flags)
"  ARGS: 
     event  - union Event, 
     widget  - object Widget, 
     path  - string, 
     background-area  - struct Rectangle, 
     cell-area  - struct Rectangle, 
     flags  - exact integer of flags type CellRendererState
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "start_editing" event widget path background-area cell-area flags)))

(define (GtkCellRenderer-stop-editing self canceled)
"  ARGS: 
     canceled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "stop_editing" canceled)))

(define <GtkCellRendererAccel>
  (gi-lookup-type "Gtk-CellRendererAccel"))

(define (GtkCellRendererAccel-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererAccel-new"))

(define CELL_RENDERER_ACCEL_MODE_GTK
  (gi-enum-value "Gtk-CellRendererAccelMode" "gtk"))

(define CELL_RENDERER_ACCEL_MODE_OTHER
  (gi-enum-value "Gtk-CellRendererAccelMode" "other"))

(define <GtkCellRendererCombo>
  (gi-lookup-type "Gtk-CellRendererCombo"))

(define (GtkCellRendererCombo-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererCombo-new"))

(define CELL_RENDERER_MODE_INERT
  (gi-enum-value "Gtk-CellRendererMode" "inert"))

(define CELL_RENDERER_MODE_ACTIVATABLE
  (gi-enum-value "Gtk-CellRendererMode" "activatable"))

(define CELL_RENDERER_MODE_EDITABLE
  (gi-enum-value "Gtk-CellRendererMode" "editable"))

(define <GtkCellRendererPixbuf>
  (gi-lookup-type "Gtk-CellRendererPixbuf"))

(define (GtkCellRendererPixbuf-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererPixbuf-new"))

(define <GtkCellRendererProgress>
  (gi-lookup-type "Gtk-CellRendererProgress"))

(define (GtkCellRendererProgress-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererProgress-new"))

(define <GtkCellRendererSpin>
  (gi-lookup-type "Gtk-CellRendererSpin"))

(define (GtkCellRendererSpin-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererSpin-new"))

(define <GtkCellRendererSpinner>
  (gi-lookup-type "Gtk-CellRendererSpinner"))

(define (GtkCellRendererSpinner-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererSpinner-new"))

(define SELECTED
  (gi-flag-value "Gtk-CellRendererState" "selected"))

(define PRELIT
  (gi-flag-value "Gtk-CellRendererState" "prelit"))

(define INSENSITIVE
  (gi-flag-value "Gtk-CellRendererState" "insensitive"))

(define SORTED
  (gi-flag-value "Gtk-CellRendererState" "sorted"))

(define FOCUSED
  (gi-flag-value "Gtk-CellRendererState" "focused"))

(define EXPANDABLE
  (gi-flag-value "Gtk-CellRendererState" "expandable"))

(define EXPANDED
  (gi-flag-value "Gtk-CellRendererState" "expanded"))

(define <GtkCellRendererText>
  (gi-lookup-type "Gtk-CellRendererText"))

(define (GtkCellRendererText-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererText-new"))

(define (GtkCellRendererText-set-fixed-height-from-font self number-of-rows)
"  ARGS: 
     number-of-rows  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fixed_height_from_font" number-of-rows)))

(define <GtkCellRendererToggle>
  (gi-lookup-type "Gtk-CellRendererToggle"))

(define (GtkCellRendererToggle-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellRendererToggle-new"))

(define (GtkCellRendererToggle-get-activatable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activatable")))

(define (GtkCellRendererToggle-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkCellRendererToggle-get-radio? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_radio")))

(define (GtkCellRendererToggle-set-activatable self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activatable" setting)))

(define (GtkCellRendererToggle-set-active self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" setting)))

(define (GtkCellRendererToggle-set-radio self radio)
"  ARGS: 
     radio  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_radio" radio)))

(define <GtkCellView>
  (gi-lookup-type "Gtk-CellView"))

(define (GtkCellView-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellView-new"))

(define (GtkCellView-new-with-context area context)
"  ARGS: 
     area  - object CellArea, 
     context  - object CellAreaContext
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellView-new_with_context" area context))

(define (GtkCellView-new-with-markup markup)
"  ARGS: 
     markup  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellView-new_with_markup" markup))

(define (GtkCellView-new-with-pixbuf pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellView-new_with_pixbuf" pixbuf))

(define (GtkCellView-new-with-text text)
"  ARGS: 
     text  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CellView-new_with_text" text))

(define (GtkCellView-get-displayed-row self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_displayed_row")))

(define (GtkCellView-get-draw-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_draw_sensitive")))

(define (GtkCellView-get-fit-model? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_fit_model")))

(define (GtkCellView-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkCellView-get-size-of-row? self path out-requisition)
"  ARGS: 
     path  - struct TreePath, 
   RETURN: gboolean
     requisition  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_size_of_row" path out-requisition)))

(define (GtkCellView-set-background-color self color)
"  ARGS: 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background_color" color)))

(define (GtkCellView-set-background-rgba self rgba)
"  ARGS: 
     rgba  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background_rgba" rgba)))

(define (GtkCellView-set-displayed-row self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_displayed_row" path)))

(define (GtkCellView-set-draw-sensitive self draw-sensitive)
"  ARGS: 
     draw-sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_draw_sensitive" draw-sensitive)))

(define (GtkCellView-set-fit-model self fit-model)
"  ARGS: 
     fit-model  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fit_model" fit-model)))

(define (GtkCellView-set-model self model)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_model" model)))

(define <GtkCheckButton>
  (gi-lookup-type "Gtk-CheckButton"))

(define (GtkCheckButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckButton-new"))

(define (GtkCheckButton-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckButton-new_with_label" label))

(define (GtkCheckButton-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckButton-new_with_mnemonic" label))

(define <GtkCheckMenuItem>
  (gi-lookup-type "Gtk-CheckMenuItem"))

(define (GtkCheckMenuItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckMenuItem-new"))

(define (GtkCheckMenuItem-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckMenuItem-new_with_label" label))

(define (GtkCheckMenuItem-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CheckMenuItem-new_with_mnemonic" label))

(define (GtkCheckMenuItem-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkCheckMenuItem-get-draw-as-radio? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_draw_as_radio")))

(define (GtkCheckMenuItem-get-inconsistent? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inconsistent")))

(define (GtkCheckMenuItem-set-active self is-active)
"  ARGS: 
     is-active  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" is-active)))

(define (GtkCheckMenuItem-set-draw-as-radio self draw-as-radio)
"  ARGS: 
     draw-as-radio  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_draw_as_radio" draw-as-radio)))

(define (GtkCheckMenuItem-set-inconsistent self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inconsistent" setting)))

(define (GtkCheckMenuItem-toggled self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toggled")))

(define <GtkCheckMenuItemAccessible>
  (gi-lookup-type "Gtk-CheckMenuItemAccessible"))

(define <GtkClipboard>
  (gi-lookup-type "Gtk-Clipboard"))

(define (GtkClipboard-get selection)
"  ARGS: 
     selection  - struct Atom
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Clipboard-get" selection))

(define (GtkClipboard-get-default display)
"  ARGS: 
     display  - object Display
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Clipboard-get_default" display))

(define (GtkClipboard-get-for-display display selection)
"  ARGS: 
     display  - object Display, 
     selection  - struct Atom
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Clipboard-get_for_display" display selection))

(define (GtkClipboard-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkClipboard-get-display self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_display")))

(define (GtkClipboard-get-owner self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_owner")))

(define (GtkClipboard-request-contents self target callback user-data)
"  ARGS: 
     target  - struct Atom, 
     callback  - procedure of type ClipboardReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_contents" target callback user-data)))

(define (GtkClipboard-request-image self callback user-data)
"  ARGS: 
     callback  - procedure of type ClipboardImageReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_image" callback user-data)))

(define (GtkClipboard-request-rich-text self buffer callback user-data)
"  ARGS: 
     buffer  - object TextBuffer, 
     callback  - procedure of type ClipboardRichTextReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_rich_text" buffer callback user-data)))

(define (GtkClipboard-request-targets self callback user-data)
"  ARGS: 
     callback  - procedure of type ClipboardTargetsReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_targets" callback user-data)))

(define (GtkClipboard-request-text self callback user-data)
"  ARGS: 
     callback  - procedure of type ClipboardTextReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_text" callback user-data)))

(define (GtkClipboard-request-uris self callback user-data)
"  ARGS: 
     callback  - procedure of type ClipboardURIReceivedFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "request_uris" callback user-data)))

(define (GtkClipboard-set-can-store self targets n-targets)
"  ARGS: 
     targets  - #f for NULL or Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_can_store" targets n-targets)))

(define (GtkClipboard-set-image self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_image" pixbuf)))

(define (GtkClipboard-set-text self text len)
"  ARGS: 
     text  - string, 
     len  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" text len)))

(define (GtkClipboard-store self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "store")))

(define (GtkClipboard-wait-for-contents self target)
"  ARGS: 
     target  - struct Atom
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_contents" target)))

(define (GtkClipboard-wait-for-image self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_image")))

(define (GtkClipboard-wait-for-rich-text self buffer out-format)
"  ARGS: 
     buffer  - object TextBuffer, 
     length  - exact integer of size guint32[OUT]
   RETURN: array*
     format  - struct Atom, 
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_rich_text" buffer out-format)))

(define (GtkClipboard-wait-for-targets? self)
"  ARGS: 
     targets  - Unhandled argument type tag 15[OUT], 
     n-targets  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_targets")))

(define (GtkClipboard-wait-for-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_text")))

(define (GtkClipboard-wait-for-uris self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "wait_for_uris")))

(define (GtkClipboard-wait-is-image-available? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_is_image_available")))

(define (GtkClipboard-wait-is-rich-text-available? self buffer)
"  ARGS: 
     buffer  - object TextBuffer
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_is_rich_text_available" buffer)))

(define (GtkClipboard-wait-is-target-available? self target)
"  ARGS: 
     target  - struct Atom
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_is_target_available" target)))

(define (GtkClipboard-wait-is-text-available? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_is_text_available")))

(define (GtkClipboard-wait-is-uris-available? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "wait_is_uris_available")))

;; CALLBACK
(define clipboard-clear-func
  (gi-lookup-callback-info "Gtk-ClipboardClearFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   user-data-or-owner  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-get-func
  (gi-lookup-callback-info "Gtk-ClipboardGetFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   selection-data  - struct SelectionData, 
;;   info  - exact integer of size guint32, 
;;   user-data-or-owner  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-image-received-func
  (gi-lookup-callback-info "Gtk-ClipboardImageReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   pixbuf  - object Pixbuf, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-received-func
  (gi-lookup-callback-info "Gtk-ClipboardReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   selection-data  - struct SelectionData, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-rich-text-received-func
  (gi-lookup-callback-info "Gtk-ClipboardRichTextReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   format  - struct Atom, 
;;   text  - #f for NULL or string, 
;;   length  - exact integer of size guint32, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-targets-received-func
  (gi-lookup-callback-info "Gtk-ClipboardTargetsReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   atoms  - #f for NULL or Unhandled argument type tag 15, 
;;   n-atoms  - exact integer of size gint32, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-text-received-func
  (gi-lookup-callback-info "Gtk-ClipboardTextReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   text  - #f for NULL or string, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define clipboard-urireceived-func
  (gi-lookup-callback-info "Gtk-ClipboardURIReceivedFunc"))
;; ARGS: 
;;   clipboard  - object Clipboard, 
;;   uris  - Unhandled argument type tag 15, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define <GtkColorButton>
  (gi-lookup-type "Gtk-ColorButton"))

(define (GtkColorButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorButton-new"))

(define (GtkColorButton-new-with-color color)
"  ARGS: 
     color  - struct Color
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorButton-new_with_color" color))

(define (GtkColorButton-new-with-rgba rgba)
"  ARGS: 
     rgba  - struct RGBA
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorButton-new_with_rgba" rgba))

(define (GtkColorButton-get-alpha self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_alpha")))

(define (GtkColorButton-get-color self out-color)
"  ARGS: 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_color" out-color)))

(define (GtkColorButton-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkColorButton-get-use-alpha? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_alpha")))

(define (GtkColorButton-set-alpha self alpha)
"  ARGS: 
     alpha  - exact integer of size guint16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alpha" alpha)))

(define (GtkColorButton-set-color self color)
"  ARGS: 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_color" color)))

(define (GtkColorButton-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkColorButton-set-use-alpha self use-alpha)
"  ARGS: 
     use-alpha  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_alpha" use-alpha)))

(define <GtkColorChooserDialog>
  (gi-lookup-type "Gtk-ColorChooserDialog"))

(define (GtkColorChooserDialog-new title parent)
"  ARGS: 
     title  - #f for NULL or string, 
     parent  - object Window
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorChooserDialog-new" title parent))

(define <GtkColorChooserWidget>
  (gi-lookup-type "Gtk-ColorChooserWidget"))

(define (GtkColorChooserWidget-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorChooserWidget-new"))

(define <GtkColorSelection>
  (gi-lookup-type "Gtk-ColorSelection"))

(define (GtkColorSelection-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorSelection-new"))

(define (GtkColorSelection-palette-from-string? str)
"  ARGS: 
     str  - string, 
     colors  - Unhandled argument type tag 15[OUT], 
     n-colors  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-ColorSelection-palette_from_string" str))

(define (GtkColorSelection-palette-to-string colors n-colors)
"  ARGS: 
     colors  - Unhandled argument type tag 15, 
     n-colors  - exact integer of size gint32
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-ColorSelection-palette_to_string" colors n-colors))

(define (GtkColorSelection-get-current-alpha self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_current_alpha")))

(define (GtkColorSelection-get-current-color self out-color)
"  ARGS: 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_current_color" out-color)))

(define (GtkColorSelection-get-current-rgba self out-rgba)
"  ARGS: 
   RETURN: void
     rgba  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_current_rgba" out-rgba)))

(define (GtkColorSelection-get-has-opacity-control? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_opacity_control")))

(define (GtkColorSelection-get-has-palette? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_palette")))

(define (GtkColorSelection-get-previous-alpha self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_previous_alpha")))

(define (GtkColorSelection-get-previous-color self out-color)
"  ARGS: 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_previous_color" out-color)))

(define (GtkColorSelection-get-previous-rgba self out-rgba)
"  ARGS: 
   RETURN: void
     rgba  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_previous_rgba" out-rgba)))

(define (GtkColorSelection-is-adjusting? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_adjusting")))

(define (GtkColorSelection-set-current-alpha self alpha)
"  ARGS: 
     alpha  - exact integer of size guint16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_alpha" alpha)))

(define (GtkColorSelection-set-current-color self color)
"  ARGS: 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_color" color)))

(define (GtkColorSelection-set-current-rgba self rgba)
"  ARGS: 
     rgba  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_rgba" rgba)))

(define (GtkColorSelection-set-has-opacity-control self has-opacity)
"  ARGS: 
     has-opacity  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_opacity_control" has-opacity)))

(define (GtkColorSelection-set-has-palette self has-palette)
"  ARGS: 
     has-palette  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_palette" has-palette)))

(define (GtkColorSelection-set-previous-alpha self alpha)
"  ARGS: 
     alpha  - exact integer of size guint16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_previous_alpha" alpha)))

(define (GtkColorSelection-set-previous-color self color)
"  ARGS: 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_previous_color" color)))

(define (GtkColorSelection-set-previous-rgba self rgba)
"  ARGS: 
     rgba  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_previous_rgba" rgba)))

(define <GtkColorSelectionDialog>
  (gi-lookup-type "Gtk-ColorSelectionDialog"))

(define (GtkColorSelectionDialog-new title)
"  ARGS: 
     title  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ColorSelectionDialog-new" title))

(define (GtkColorSelectionDialog-get-color-selection self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_color_selection")))

(define <GtkComboBox>
  (gi-lookup-type "Gtk-ComboBox"))

(define (GtkComboBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new"))

(define (GtkComboBox-new-with-area area)
"  ARGS: 
     area  - object CellArea
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new_with_area" area))

(define (GtkComboBox-new-with-area-and-entry area)
"  ARGS: 
     area  - object CellArea
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new_with_area_and_entry" area))

(define (GtkComboBox-new-with-entry)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new_with_entry"))

(define (GtkComboBox-new-with-model model)
"  ARGS: 
     model  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new_with_model" model))

(define (GtkComboBox-new-with-model-and-entry model)
"  ARGS: 
     model  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBox-new_with_model_and_entry" model))

(define (GtkComboBox-get-active self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkComboBox-get-active-id self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_active_id")))

(define (GtkComboBox-get-active-iter? self out-iter)
"  ARGS: 
   RETURN: gboolean
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_active_iter" out-iter)))

(define (GtkComboBox-get-add-tearoffs? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_add_tearoffs")))

(define (GtkComboBox-get-button-sensitivity self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_button_sensitivity")))

(define (GtkComboBox-get-column-span-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_column_span_column")))

(define (GtkComboBox-get-entry-text-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_entry_text_column")))

(define (GtkComboBox-get-focus-on-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_on_click")))

(define (GtkComboBox-get-has-entry? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_entry")))

(define (GtkComboBox-get-id-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_id_column")))

(define (GtkComboBox-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkComboBox-get-popup-accessible self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_popup_accessible")))

(define (GtkComboBox-get-popup-fixed-width? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_popup_fixed_width")))

(define (GtkComboBox-get-row-span-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_row_span_column")))

(define (GtkComboBox-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkComboBox-get-wrap-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_wrap_width")))

(define (GtkComboBox-popdown self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popdown")))

(define (GtkComboBox-popup self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup")))

(define (GtkComboBox-popup-for-device self device)
"  ARGS: 
     device  - object Device
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_for_device" device)))

(define (GtkComboBox-set-active self index-)
"  ARGS: 
     index-  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" index-)))

(define (GtkComboBox-set-active-id? self active-id)
"  ARGS: 
     active-id  - #f for NULL or string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_active_id" active-id)))

(define (GtkComboBox-set-active-iter self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active_iter" iter)))

(define (GtkComboBox-set-add-tearoffs self add-tearoffs)
"  ARGS: 
     add-tearoffs  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_add_tearoffs" add-tearoffs)))

(define (GtkComboBox-set-button-sensitivity self sensitivity)
"  ARGS: 
     sensitivity  - exact integer of enum type SensitivityType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_button_sensitivity" sensitivity)))

(define (GtkComboBox-set-column-span-column self column-span)
"  ARGS: 
     column-span  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_span_column" column-span)))

(define (GtkComboBox-set-entry-text-column self text-column)
"  ARGS: 
     text-column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_entry_text_column" text-column)))

(define (GtkComboBox-set-focus-on-click self focus-on-click)
"  ARGS: 
     focus-on-click  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_on_click" focus-on-click)))

(define (GtkComboBox-set-id-column self id-column)
"  ARGS: 
     id-column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_id_column" id-column)))

(define (GtkComboBox-set-model self model)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_model" model)))

(define (GtkComboBox-set-popup-fixed-width self fixed)
"  ARGS: 
     fixed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popup_fixed_width" fixed)))

(define (GtkComboBox-set-row-separator-func self func data destroy)
"  ARGS: 
     func  - procedure of type TreeViewRowSeparatorFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_separator_func" func data destroy)))

(define (GtkComboBox-set-row-span-column self row-span)
"  ARGS: 
     row-span  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_span_column" row-span)))

(define (GtkComboBox-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkComboBox-set-wrap-width self width)
"  ARGS: 
     width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wrap_width" width)))

(define <GtkComboBoxAccessible>
  (gi-lookup-type "Gtk-ComboBoxAccessible"))

(define <GtkComboBoxText>
  (gi-lookup-type "Gtk-ComboBoxText"))

(define (GtkComboBoxText-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBoxText-new"))

(define (GtkComboBoxText-new-with-entry)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ComboBoxText-new_with_entry"))

(define (GtkComboBoxText-append self id text)
"  ARGS: 
     id  - #f for NULL or string, 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append" id text)))

(define (GtkComboBoxText-append-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_text" text)))

(define (GtkComboBoxText-get-active-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_active_text")))

(define (GtkComboBoxText-insert self position id text)
"  ARGS: 
     position  - exact integer of size gint32, 
     id  - #f for NULL or string, 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" position id text)))

(define (GtkComboBoxText-insert-text self position text)
"  ARGS: 
     position  - exact integer of size gint32, 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_text" position text)))

(define (GtkComboBoxText-prepend self id text)
"  ARGS: 
     id  - #f for NULL or string, 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend" id text)))

(define (GtkComboBoxText-prepend-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend_text" text)))

(define (GtkComboBoxText-remove self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" position)))

(define (GtkComboBoxText-remove-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_all")))

(define <GtkContainer>
  (gi-lookup-type "Gtk-Container"))

(define (GtkContainer-add self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add" widget)))

(define (GtkContainer-check-resize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "check_resize")))

(define (GtkContainer-child-get-property self child property-name value)
"  ARGS: 
     child  - object Widget, 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "child_get_property" child property-name value)))

(define (GtkContainer-child-notify self child child-property)
"  ARGS: 
     child  - object Widget, 
     child-property  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "child_notify" child child-property)))

(define (GtkContainer-child-notify-by-pspec self child pspec)
"  ARGS: 
     child  - object Widget, 
     pspec  - object ParamSpec
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "child_notify_by_pspec" child pspec)))

(define (GtkContainer-child-set-property self child property-name value)
"  ARGS: 
     child  - object Widget, 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "child_set_property" child property-name value)))

(define (GtkContainer-child-type self)
"  ARGS: 
   RETURN: GType
"
  (gi-method-send self 
     (gi-method-prepare "child_type")))

(define (GtkContainer-forall self callback callback-data)
"  ARGS: 
     callback  - procedure of type Callback, 
     callback-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "forall" callback callback-data)))

(define (GtkContainer-foreach self callback callback-data)
"  ARGS: 
     callback  - procedure of type Callback, 
     callback-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "foreach" callback callback-data)))

(define (GtkContainer-get-border-width self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_border_width")))

(define (GtkContainer-get-children self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_children")))

(define (GtkContainer-get-focus-chain? self)
"  ARGS: 
     focusable-widgets  - <GList>[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_chain")))

(define (GtkContainer-get-focus-child self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_child")))

(define (GtkContainer-get-focus-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_hadjustment")))

(define (GtkContainer-get-focus-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_vadjustment")))

(define (GtkContainer-get-path-for-child self child)
"  ARGS: 
     child  - object Widget
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path_for_child" child)))

(define (GtkContainer-get-resize-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_resize_mode")))

(define (GtkContainer-propagate-draw self child cr)
"  ARGS: 
     child  - object Widget, 
     cr  - struct Context
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "propagate_draw" child cr)))

(define (GtkContainer-remove self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" widget)))

(define (GtkContainer-resize-children self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "resize_children")))

(define (GtkContainer-set-border-width self border-width)
"  ARGS: 
     border-width  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_border_width" border-width)))

(define (GtkContainer-set-focus-chain self focusable-widgets)
"  ARGS: 
     focusable-widgets  - <GList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_chain" focusable-widgets)))

(define (GtkContainer-set-focus-child self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_child" child)))

(define (GtkContainer-set-focus-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_hadjustment" adjustment)))

(define (GtkContainer-set-focus-vadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_vadjustment" adjustment)))

(define (GtkContainer-set-reallocate-redraws self needs-redraws)
"  ARGS: 
     needs-redraws  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reallocate_redraws" needs-redraws)))

(define (GtkContainer-set-resize-mode self resize-mode)
"  ARGS: 
     resize-mode  - exact integer of enum type ResizeMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resize_mode" resize-mode)))

(define (GtkContainer-unset-focus-chain self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_focus_chain")))

(define <GtkContainerAccessible>
  (gi-lookup-type "Gtk-ContainerAccessible"))

(define <GtkContainerCellAccessible>
  (gi-lookup-type "Gtk-ContainerCellAccessible"))

(define (GtkContainerCellAccessible-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ContainerCellAccessible-new"))

(define (GtkContainerCellAccessible-add-child self child)
"  ARGS: 
     child  - object CellAccessible
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_child" child)))

(define (GtkContainerCellAccessible-get-children self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_children")))

(define (GtkContainerCellAccessible-remove-child self child)
"  ARGS: 
     child  - object CellAccessible
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_child" child)))

(define CORNER_TYPE_TOP_LEFT
  (gi-enum-value "Gtk-CornerType" "top_left"))

(define CORNER_TYPE_BOTTOM_LEFT
  (gi-enum-value "Gtk-CornerType" "bottom_left"))

(define CORNER_TYPE_TOP_RIGHT
  (gi-enum-value "Gtk-CornerType" "top_right"))

(define CORNER_TYPE_BOTTOM_RIGHT
  (gi-enum-value "Gtk-CornerType" "bottom_right"))

(define <GtkCssProvider>
  (gi-lookup-type "Gtk-CssProvider"))

(define (GtkCssProvider-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CssProvider-new"))

(define (GtkCssProvider-get-default)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CssProvider-get_default"))

(define (GtkCssProvider-get-named name variant)
"  ARGS: 
     name  - string, 
     variant  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-CssProvider-get_named" name variant))

(define (GtkCssProvider-load-from-data? self data length)
"  ARGS: 
     data  - Unhandled argument type tag 15, 
     length  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_from_data" data length)))

(define (GtkCssProvider-load-from-file? self file)
"  ARGS: 
     file  - Unhandled argument type tag 16
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_from_file" file)))

(define (GtkCssProvider-load-from-path? self path)
"  ARGS: 
     path  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_from_path" path)))

(define (GtkCssProvider-load-from-resource self resource-path)
"  ARGS: 
     resource-path  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "load_from_resource" resource-path)))

(define (GtkCssProvider-to-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string")))

(define CSS_PROVIDER_ERROR_FAILED
  (gi-enum-value "Gtk-CssProviderError" "failed"))

(define CSS_PROVIDER_ERROR_SYNTAX
  (gi-enum-value "Gtk-CssProviderError" "syntax"))

(define CSS_PROVIDER_ERROR_IMPORT
  (gi-enum-value "Gtk-CssProviderError" "import"))

(define CSS_PROVIDER_ERROR_NAME
  (gi-enum-value "Gtk-CssProviderError" "name"))

(define CSS_PROVIDER_ERROR_DEPRECATED
  (gi-enum-value "Gtk-CssProviderError" "deprecated"))

(define CSS_PROVIDER_ERROR_UNKNOWN_VALUE
  (gi-enum-value "Gtk-CssProviderError" "unknown_value"))

(define <GtkCssSection>
  (gi-lookup-type "Gtk-CssSection"))

(define (GtkCssSection-get-end-line self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_end_line")))

(define (GtkCssSection-get-end-position self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_end_position")))

(define (GtkCssSection-get-file self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_file")))

(define (GtkCssSection-get-parent self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent")))

(define (GtkCssSection-get-section-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_section_type")))

(define (GtkCssSection-get-start-line self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_start_line")))

(define (GtkCssSection-get-start-position self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_start_position")))

(define (GtkCssSection-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkCssSection-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define CSS_SECTION_TYPE_DOCUMENT
  (gi-enum-value "Gtk-CssSectionType" "document"))

(define CSS_SECTION_TYPE_IMPORT
  (gi-enum-value "Gtk-CssSectionType" "import"))

(define CSS_SECTION_TYPE_COLOR_DEFINITION
  (gi-enum-value "Gtk-CssSectionType" "color_definition"))

(define CSS_SECTION_TYPE_BINDING_SET
  (gi-enum-value "Gtk-CssSectionType" "binding_set"))

(define CSS_SECTION_TYPE_RULESET
  (gi-enum-value "Gtk-CssSectionType" "ruleset"))

(define CSS_SECTION_TYPE_SELECTOR
  (gi-enum-value "Gtk-CssSectionType" "selector"))

(define CSS_SECTION_TYPE_DECLARATION
  (gi-enum-value "Gtk-CssSectionType" "declaration"))

(define CSS_SECTION_TYPE_VALUE
  (gi-enum-value "Gtk-CssSectionType" "value"))

(define CSS_SECTION_TYPE_KEYFRAMES
  (gi-enum-value "Gtk-CssSectionType" "keyframes"))

(define MISC
  (gi-flag-value "Gtk-DebugFlag" "misc"))

(define PLUGSOCKET
  (gi-flag-value "Gtk-DebugFlag" "plugsocket"))

(define TEXT
  (gi-flag-value "Gtk-DebugFlag" "text"))

(define TREE
  (gi-flag-value "Gtk-DebugFlag" "tree"))

(define UPDATES
  (gi-flag-value "Gtk-DebugFlag" "updates"))

(define KEYBINDINGS
  (gi-flag-value "Gtk-DebugFlag" "keybindings"))

(define MULTIHEAD
  (gi-flag-value "Gtk-DebugFlag" "multihead"))

(define MODULES
  (gi-flag-value "Gtk-DebugFlag" "modules"))

(define GEOMETRY
  (gi-flag-value "Gtk-DebugFlag" "geometry"))

(define ICONTHEME
  (gi-flag-value "Gtk-DebugFlag" "icontheme"))

(define PRINTING
  (gi-flag-value "Gtk-DebugFlag" "printing"))

(define BUILDER
  (gi-flag-value "Gtk-DebugFlag" "builder"))

(define SIZE_REQUEST
  (gi-flag-value "Gtk-DebugFlag" "size_request"))

(define NO_CSS_CACHE
  (gi-flag-value "Gtk-DebugFlag" "no_css_cache"))

(define BASELINES
  (gi-flag-value "Gtk-DebugFlag" "baselines"))

(define PIXEL_CACHE
  (gi-flag-value "Gtk-DebugFlag" "pixel_cache"))

(define NO_PIXEL_CACHE
  (gi-flag-value "Gtk-DebugFlag" "no_pixel_cache"))

(define INTERACTIVE
  (gi-flag-value "Gtk-DebugFlag" "interactive"))

(define TOUCHSCREEN
  (gi-flag-value "Gtk-DebugFlag" "touchscreen"))

(define ACTIONS
  (gi-flag-value "Gtk-DebugFlag" "actions"))

(define RESIZE
  (gi-flag-value "Gtk-DebugFlag" "resize"))

(define LAYOUT
  (gi-flag-value "Gtk-DebugFlag" "layout"))

(define DELETE_TYPE_CHARS
  (gi-enum-value "Gtk-DeleteType" "chars"))

(define DELETE_TYPE_WORD_ENDS
  (gi-enum-value "Gtk-DeleteType" "word_ends"))

(define DELETE_TYPE_WORDS
  (gi-enum-value "Gtk-DeleteType" "words"))

(define DELETE_TYPE_DISPLAY_LINES
  (gi-enum-value "Gtk-DeleteType" "display_lines"))

(define DELETE_TYPE_DISPLAY_LINE_ENDS
  (gi-enum-value "Gtk-DeleteType" "display_line_ends"))

(define DELETE_TYPE_PARAGRAPH_ENDS
  (gi-enum-value "Gtk-DeleteType" "paragraph_ends"))

(define DELETE_TYPE_PARAGRAPHS
  (gi-enum-value "Gtk-DeleteType" "paragraphs"))

(define DELETE_TYPE_WHITESPACE
  (gi-enum-value "Gtk-DeleteType" "whitespace"))

(define MOTION
  (gi-flag-value "Gtk-DestDefaults" "motion"))

(define HIGHLIGHT
  (gi-flag-value "Gtk-DestDefaults" "highlight"))

(define DROP
  (gi-flag-value "Gtk-DestDefaults" "drop"))

(define ALL
  (gi-flag-value "Gtk-DestDefaults" "all"))

(define <GtkDialog>
  (gi-lookup-type "Gtk-Dialog"))

(define (GtkDialog-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Dialog-new"))

(define (GtkDialog-add-action-widget self child response-id)
"  ARGS: 
     child  - object Widget, 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_action_widget" child response-id)))

(define (GtkDialog-add-button self button-text response-id)
"  ARGS: 
     button-text  - string, 
     response-id  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "add_button" button-text response-id)))

(define (GtkDialog-get-action-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action_area")))

(define (GtkDialog-get-content-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_content_area")))

(define (GtkDialog-get-header-bar self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_header_bar")))

(define (GtkDialog-get-response-for-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_response_for_widget" widget)))

(define (GtkDialog-get-widget-for-response self response-id)
"  ARGS: 
     response-id  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget_for_response" response-id)))

(define (GtkDialog-response self response-id)
"  ARGS: 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "response" response-id)))

(define (GtkDialog-run self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "run")))

(define (GtkDialog-set-alternative-button-order-from-array self n-params new-order)
"  ARGS: 
     n-params  - exact integer of size gint32, 
     new-order  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alternative_button_order_from_array" n-params new-order)))

(define (GtkDialog-set-default-response self response-id)
"  ARGS: 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_response" response-id)))

(define (GtkDialog-set-response-sensitive self response-id setting)
"  ARGS: 
     response-id  - exact integer of size gint32, 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_response_sensitive" response-id setting)))

(define MODAL
  (gi-flag-value "Gtk-DialogFlags" "modal"))

(define DESTROY_WITH_PARENT
  (gi-flag-value "Gtk-DialogFlags" "destroy_with_parent"))

(define USE_HEADER_BAR
  (gi-flag-value "Gtk-DialogFlags" "use_header_bar"))

(define DIRECTION_TYPE_TAB_FORWARD
  (gi-enum-value "Gtk-DirectionType" "tab_forward"))

(define DIRECTION_TYPE_TAB_BACKWARD
  (gi-enum-value "Gtk-DirectionType" "tab_backward"))

(define DIRECTION_TYPE_UP
  (gi-enum-value "Gtk-DirectionType" "up"))

(define DIRECTION_TYPE_DOWN
  (gi-enum-value "Gtk-DirectionType" "down"))

(define DIRECTION_TYPE_LEFT
  (gi-enum-value "Gtk-DirectionType" "left"))

(define DIRECTION_TYPE_RIGHT
  (gi-enum-value "Gtk-DirectionType" "right"))

(define DRAG_RESULT_SUCCESS
  (gi-enum-value "Gtk-DragResult" "success"))

(define DRAG_RESULT_NO_TARGET
  (gi-enum-value "Gtk-DragResult" "no_target"))

(define DRAG_RESULT_USER_CANCELLED
  (gi-enum-value "Gtk-DragResult" "user_cancelled"))

(define DRAG_RESULT_TIMEOUT_EXPIRED
  (gi-enum-value "Gtk-DragResult" "timeout_expired"))

(define DRAG_RESULT_GRAB_BROKEN
  (gi-enum-value "Gtk-DragResult" "grab_broken"))

(define DRAG_RESULT_ERROR
  (gi-enum-value "Gtk-DragResult" "error"))

(define <GtkDrawingArea>
  (gi-lookup-type "Gtk-DrawingArea"))

(define (GtkDrawingArea-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-DrawingArea-new"))

(define <GtkEntry>
  (gi-lookup-type "Gtk-Entry"))

(define (GtkEntry-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Entry-new"))

(define (GtkEntry-new-with-buffer buffer)
"  ARGS: 
     buffer  - object EntryBuffer
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Entry-new_with_buffer" buffer))

(define (GtkEntry-get-activates-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activates_default")))

(define (GtkEntry-get-alignment self)
"  ARGS: 
   RETURN: gfloat
"
  (gi-method-send self 
     (gi-method-prepare "get_alignment")))

(define (GtkEntry-get-attributes self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_attributes")))

(define (GtkEntry-get-buffer self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_buffer")))

(define (GtkEntry-get-completion self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_completion")))

(define (GtkEntry-get-current-icon-drag-source self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_current_icon_drag_source")))

(define (GtkEntry-get-cursor-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_cursor_hadjustment")))

(define (GtkEntry-get-has-frame? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_frame")))

(define (GtkEntry-get-icon-activatable? self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_activatable" icon-pos)))

(define (GtkEntry-get-icon-area self icon-pos out-icon-area)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
   RETURN: void
     icon-area  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_area" icon-pos out-icon-area)))

(define (GtkEntry-get-icon-at-pos self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_at_pos" x y)))

(define (GtkEntry-get-icon-gicon self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_gicon" icon-pos)))

(define (GtkEntry-get-icon-name self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name" icon-pos)))

(define (GtkEntry-get-icon-pixbuf self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_pixbuf" icon-pos)))

(define (GtkEntry-get-icon-sensitive? self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_sensitive" icon-pos)))

(define (GtkEntry-get-icon-stock self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_stock" icon-pos)))

(define (GtkEntry-get-icon-storage-type self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_storage_type" icon-pos)))

(define (GtkEntry-get-icon-tooltip-markup self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_tooltip_markup" icon-pos)))

(define (GtkEntry-get-icon-tooltip-text self icon-pos)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_tooltip_text" icon-pos)))

(define (GtkEntry-get-inner-border self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_inner_border")))

(define (GtkEntry-get-input-hints self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_input_hints")))

(define (GtkEntry-get-input-purpose self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_input_purpose")))

(define (GtkEntry-get-invisible-char self)
"  ARGS: 
   RETURN: gunichar
"
  (gi-method-send self 
     (gi-method-prepare "get_invisible_char")))

(define (GtkEntry-get-layout self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_layout")))

(define (GtkEntry-get-layout-offsets self)
"  ARGS: 
     x  - exact integer of size gint32[OUT], 
     y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_layout_offsets")))

(define (GtkEntry-get-max-length self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_length")))

(define (GtkEntry-get-max-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_width_chars")))

(define (GtkEntry-get-overwrite-mode? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_overwrite_mode")))

(define (GtkEntry-get-placeholder-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_placeholder_text")))

(define (GtkEntry-get-progress-fraction self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_progress_fraction")))

(define (GtkEntry-get-progress-pulse-step self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_progress_pulse_step")))

(define (GtkEntry-get-tabs self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tabs")))

(define (GtkEntry-get-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text")))

(define (GtkEntry-get-text-area self out-text-area)
"  ARGS: 
   RETURN: void
     text-area  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_text_area" out-text-area)))

(define (GtkEntry-get-text-length self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_text_length")))

(define (GtkEntry-get-visibility? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visibility")))

(define (GtkEntry-get-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_width_chars")))

(define (GtkEntry-grab-focus-without-selecting self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "grab_focus_without_selecting")))

(define (GtkEntry-im-context-filter-keypress? self event)
"  ARGS: 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "im_context_filter_keypress" event)))

(define (GtkEntry-layout-index-to-text-index self layout-index)
"  ARGS: 
     layout-index  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "layout_index_to_text_index" layout-index)))

(define (GtkEntry-progress-pulse self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "progress_pulse")))

(define (GtkEntry-reset-im-context self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_im_context")))

(define (GtkEntry-set-activates-default self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activates_default" setting)))

(define (GtkEntry-set-alignment self xalign)
"  ARGS: 
     xalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alignment" xalign)))

(define (GtkEntry-set-attributes self attrs)
"  ARGS: 
     attrs  - struct AttrList
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_attributes" attrs)))

(define (GtkEntry-set-buffer self buffer)
"  ARGS: 
     buffer  - object EntryBuffer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_buffer" buffer)))

(define (GtkEntry-set-completion self completion)
"  ARGS: 
     completion  - object EntryCompletion
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_completion" completion)))

(define (GtkEntry-set-cursor-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor_hadjustment" adjustment)))

(define (GtkEntry-set-has-frame self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_frame" setting)))

(define (GtkEntry-set-icon-activatable self icon-pos activatable)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     activatable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_activatable" icon-pos activatable)))

(define (GtkEntry-set-icon-drag-source self icon-pos target-list actions)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     target-list  - struct TargetList, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_drag_source" icon-pos target-list actions)))

(define (GtkEntry-set-icon-from-gicon self icon-pos icon)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     icon  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_gicon" icon-pos icon)))

(define (GtkEntry-set-icon-from-icon-name self icon-pos icon-name)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     icon-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_icon_name" icon-pos icon-name)))

(define (GtkEntry-set-icon-from-pixbuf self icon-pos pixbuf)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_pixbuf" icon-pos pixbuf)))

(define (GtkEntry-set-icon-from-stock self icon-pos stock-id)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     stock-id  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_stock" icon-pos stock-id)))

(define (GtkEntry-set-icon-sensitive self icon-pos sensitive)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_sensitive" icon-pos sensitive)))

(define (GtkEntry-set-icon-tooltip-markup self icon-pos tooltip)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     tooltip  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_tooltip_markup" icon-pos tooltip)))

(define (GtkEntry-set-icon-tooltip-text self icon-pos tooltip)
"  ARGS: 
     icon-pos  - exact integer of enum type EntryIconPosition, 
     tooltip  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_tooltip_text" icon-pos tooltip)))

(define (GtkEntry-set-inner-border self border)
"  ARGS: 
     border  - struct Border
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inner_border" border)))

(define (GtkEntry-set-input-hints self hints)
"  ARGS: 
     hints  - exact integer of flags type InputHints
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_input_hints" hints)))

(define (GtkEntry-set-input-purpose self purpose)
"  ARGS: 
     purpose  - exact integer of enum type InputPurpose
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_input_purpose" purpose)))

(define (GtkEntry-set-invisible-char self ch)
"  ARGS: 
     ch  - character
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_invisible_char" ch)))

(define (GtkEntry-set-max-length self max)
"  ARGS: 
     max  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_length" max)))

(define (GtkEntry-set-max-width-chars self n-chars)
"  ARGS: 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_width_chars" n-chars)))

(define (GtkEntry-set-overwrite-mode self overwrite)
"  ARGS: 
     overwrite  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_overwrite_mode" overwrite)))

(define (GtkEntry-set-placeholder-text self text)
"  ARGS: 
     text  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_placeholder_text" text)))

(define (GtkEntry-set-progress-fraction self fraction)
"  ARGS: 
     fraction  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_progress_fraction" fraction)))

(define (GtkEntry-set-progress-pulse-step self fraction)
"  ARGS: 
     fraction  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_progress_pulse_step" fraction)))

(define (GtkEntry-set-tabs self tabs)
"  ARGS: 
     tabs  - struct TabArray
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tabs" tabs)))

(define (GtkEntry-set-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" text)))

(define (GtkEntry-set-visibility self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visibility" visible)))

(define (GtkEntry-set-width-chars self n-chars)
"  ARGS: 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_width_chars" n-chars)))

(define (GtkEntry-text-index-to-layout-index self text-index)
"  ARGS: 
     text-index  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "text_index_to_layout_index" text-index)))

(define (GtkEntry-unset-invisible-char self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_invisible_char")))

(define <GtkEntryAccessible>
  (gi-lookup-type "Gtk-EntryAccessible"))

(define <GtkEntryBuffer>
  (gi-lookup-type "Gtk-EntryBuffer"))

(define (GtkEntryBuffer-new initial-chars n-initial-chars)
"  ARGS: 
     initial-chars  - #f for NULL or string, 
     n-initial-chars  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EntryBuffer-new" initial-chars n-initial-chars))

(define (GtkEntryBuffer-delete-text self position n-chars)
"  ARGS: 
     position  - exact integer of size guint32, 
     n-chars  - exact integer of size gint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "delete_text" position n-chars)))

(define (GtkEntryBuffer-emit-deleted-text self position n-chars)
"  ARGS: 
     position  - exact integer of size guint32, 
     n-chars  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "emit_deleted_text" position n-chars)))

(define (GtkEntryBuffer-emit-inserted-text self position chars n-chars)
"  ARGS: 
     position  - exact integer of size guint32, 
     chars  - string, 
     n-chars  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "emit_inserted_text" position chars n-chars)))

(define (GtkEntryBuffer-get-bytes self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_bytes")))

(define (GtkEntryBuffer-get-length self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_length")))

(define (GtkEntryBuffer-get-max-length self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_length")))

(define (GtkEntryBuffer-get-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text")))

(define (GtkEntryBuffer-insert-text self position chars n-chars)
"  ARGS: 
     position  - exact integer of size guint32, 
     chars  - string, 
     n-chars  - exact integer of size gint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_text" position chars n-chars)))

(define (GtkEntryBuffer-set-max-length self max-length)
"  ARGS: 
     max-length  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_length" max-length)))

(define (GtkEntryBuffer-set-text self chars n-chars)
"  ARGS: 
     chars  - string, 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" chars n-chars)))

(define <GtkEntryCompletion>
  (gi-lookup-type "Gtk-EntryCompletion"))

(define (GtkEntryCompletion-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EntryCompletion-new"))

(define (GtkEntryCompletion-new-with-area area)
"  ARGS: 
     area  - object CellArea
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EntryCompletion-new_with_area" area))

(define (GtkEntryCompletion-complete self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "complete")))

(define (GtkEntryCompletion-compute-prefix self key)
"  ARGS: 
     key  - string
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "compute_prefix" key)))

(define (GtkEntryCompletion-delete-action self index-)
"  ARGS: 
     index-  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "delete_action" index-)))

(define (GtkEntryCompletion-get-completion-prefix self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_completion_prefix")))

(define (GtkEntryCompletion-get-entry self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_entry")))

(define (GtkEntryCompletion-get-inline-completion? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inline_completion")))

(define (GtkEntryCompletion-get-inline-selection? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inline_selection")))

(define (GtkEntryCompletion-get-minimum-key-length self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_minimum_key_length")))

(define (GtkEntryCompletion-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkEntryCompletion-get-popup-completion? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_popup_completion")))

(define (GtkEntryCompletion-get-popup-set-width? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_popup_set_width")))

(define (GtkEntryCompletion-get-popup-single-match? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_popup_single_match")))

(define (GtkEntryCompletion-get-text-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_text_column")))

(define (GtkEntryCompletion-insert-action-markup self index- markup)
"  ARGS: 
     index-  - exact integer of size gint32, 
     markup  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_action_markup" index- markup)))

(define (GtkEntryCompletion-insert-action-text self index- text)
"  ARGS: 
     index-  - exact integer of size gint32, 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_action_text" index- text)))

(define (GtkEntryCompletion-insert-prefix self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_prefix")))

(define (GtkEntryCompletion-set-inline-completion self inline-completion)
"  ARGS: 
     inline-completion  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inline_completion" inline-completion)))

(define (GtkEntryCompletion-set-inline-selection self inline-selection)
"  ARGS: 
     inline-selection  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inline_selection" inline-selection)))

(define (GtkEntryCompletion-set-match-func self func func-data func-notify)
"  ARGS: 
     func  - procedure of type EntryCompletionMatchFunc, 
     func-data  - #f for NULL or pointer, 
     func-notify  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_match_func" func func-data func-notify)))

(define (GtkEntryCompletion-set-minimum-key-length self length)
"  ARGS: 
     length  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_minimum_key_length" length)))

(define (GtkEntryCompletion-set-model self model)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_model" model)))

(define (GtkEntryCompletion-set-popup-completion self popup-completion)
"  ARGS: 
     popup-completion  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popup_completion" popup-completion)))

(define (GtkEntryCompletion-set-popup-set-width self popup-set-width)
"  ARGS: 
     popup-set-width  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popup_set_width" popup-set-width)))

(define (GtkEntryCompletion-set-popup-single-match self popup-single-match)
"  ARGS: 
     popup-single-match  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popup_single_match" popup-single-match)))

(define (GtkEntryCompletion-set-text-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text_column" column)))

;; CALLBACK
(define entry-completion-match-func
  (gi-lookup-callback-info "Gtk-EntryCompletionMatchFunc"))
;; ARGS: 
;;   completion  - object EntryCompletion, 
;;   key  - string, 
;;   iter  - struct TreeIter, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkEntryIconAccessible>
  (gi-lookup-type "Gtk-EntryIconAccessible"))

(define ENTRY_ICON_POSITION_PRIMARY
  (gi-enum-value "Gtk-EntryIconPosition" "primary"))

(define ENTRY_ICON_POSITION_SECONDARY
  (gi-enum-value "Gtk-EntryIconPosition" "secondary"))

(define <GtkEventBox>
  (gi-lookup-type "Gtk-EventBox"))

(define (GtkEventBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EventBox-new"))

(define (GtkEventBox-get-above-child? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_above_child")))

(define (GtkEventBox-get-visible-window? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_window")))

(define (GtkEventBox-set-above-child self above-child)
"  ARGS: 
     above-child  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_above_child" above-child)))

(define (GtkEventBox-set-visible-window self visible-window)
"  ARGS: 
     visible-window  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_window" visible-window)))

(define <GtkEventController>
  (gi-lookup-type "Gtk-EventController"))

(define (GtkEventController-get-propagation-phase self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_propagation_phase")))

(define (GtkEventController-get-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget")))

(define (GtkEventController-handle-event? self event)
"  ARGS: 
     event  - union Event
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "handle_event" event)))

(define (GtkEventController-reset self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset")))

(define (GtkEventController-set-propagation-phase self phase)
"  ARGS: 
     phase  - exact integer of enum type PropagationPhase
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_propagation_phase" phase)))

(define <GtkEventControllerKey>
  (gi-lookup-type "Gtk-EventControllerKey"))

(define (GtkEventControllerKey-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EventControllerKey-new" widget))

(define (GtkEventControllerKey-forward? self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward" widget)))

(define (GtkEventControllerKey-get-group self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkEventControllerKey-get-im-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_im_context")))

(define (GtkEventControllerKey-set-im-context self im-context)
"  ARGS: 
     im-context  - object IMContext
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_im_context" im-context)))

(define <GtkEventControllerMotion>
  (gi-lookup-type "Gtk-EventControllerMotion"))

(define (GtkEventControllerMotion-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EventControllerMotion-new" widget))

(define <GtkEventControllerScroll>
  (gi-lookup-type "Gtk-EventControllerScroll"))

(define (GtkEventControllerScroll-new widget flags)
"  ARGS: 
     widget  - object Widget, 
     flags  - exact integer of flags type EventControllerScrollFlags
   RETURN: interface*
"
  (gi-function-invoke "Gtk-EventControllerScroll-new" widget flags))

(define (GtkEventControllerScroll-get-flags self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_flags")))

(define (GtkEventControllerScroll-set-flags self flags)
"  ARGS: 
     flags  - exact integer of flags type EventControllerScrollFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_flags" flags)))

(define NONE
  (gi-flag-value "Gtk-EventControllerScrollFlags" "none"))

(define VERTICAL
  (gi-flag-value "Gtk-EventControllerScrollFlags" "vertical"))

(define HORIZONTAL
  (gi-flag-value "Gtk-EventControllerScrollFlags" "horizontal"))

(define DISCRETE
  (gi-flag-value "Gtk-EventControllerScrollFlags" "discrete"))

(define KINETIC
  (gi-flag-value "Gtk-EventControllerScrollFlags" "kinetic"))

(define BOTH_AXES
  (gi-flag-value "Gtk-EventControllerScrollFlags" "both_axes"))

(define EVENT_SEQUENCE_STATE_NONE
  (gi-enum-value "Gtk-EventSequenceState" "none"))

(define EVENT_SEQUENCE_STATE_CLAIMED
  (gi-enum-value "Gtk-EventSequenceState" "claimed"))

(define EVENT_SEQUENCE_STATE_DENIED
  (gi-enum-value "Gtk-EventSequenceState" "denied"))

(define <GtkExpander>
  (gi-lookup-type "Gtk-Expander"))

(define (GtkExpander-new label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Expander-new" label))

(define (GtkExpander-new-with-mnemonic label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Expander-new_with_mnemonic" label))

(define (GtkExpander-get-expanded? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_expanded")))

(define (GtkExpander-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkExpander-get-label-fill? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_label_fill")))

(define (GtkExpander-get-label-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_label_widget")))

(define (GtkExpander-get-resize-toplevel? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_resize_toplevel")))

(define (GtkExpander-get-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_spacing")))

(define (GtkExpander-get-use-markup? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_markup")))

(define (GtkExpander-get-use-underline? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_underline")))

(define (GtkExpander-set-expanded self expanded)
"  ARGS: 
     expanded  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_expanded" expanded)))

(define (GtkExpander-set-label self label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkExpander-set-label-fill self label-fill)
"  ARGS: 
     label-fill  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_fill" label-fill)))

(define (GtkExpander-set-label-widget self label-widget)
"  ARGS: 
     label-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_widget" label-widget)))

(define (GtkExpander-set-resize-toplevel self resize-toplevel)
"  ARGS: 
     resize-toplevel  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resize_toplevel" resize-toplevel)))

(define (GtkExpander-set-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_spacing" spacing)))

(define (GtkExpander-set-use-markup self use-markup)
"  ARGS: 
     use-markup  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_markup" use-markup)))

(define (GtkExpander-set-use-underline self use-underline)
"  ARGS: 
     use-underline  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_underline" use-underline)))

(define <GtkExpanderAccessible>
  (gi-lookup-type "Gtk-ExpanderAccessible"))

(define EXPANDER_STYLE_COLLAPSED
  (gi-enum-value "Gtk-ExpanderStyle" "collapsed"))

(define EXPANDER_STYLE_SEMI_COLLAPSED
  (gi-enum-value "Gtk-ExpanderStyle" "semi_collapsed"))

(define EXPANDER_STYLE_SEMI_EXPANDED
  (gi-enum-value "Gtk-ExpanderStyle" "semi_expanded"))

(define EXPANDER_STYLE_EXPANDED
  (gi-enum-value "Gtk-ExpanderStyle" "expanded"))

(define FILE_CHOOSER_ACTION_OPEN
  (gi-enum-value "Gtk-FileChooserAction" "open"))

(define FILE_CHOOSER_ACTION_SAVE
  (gi-enum-value "Gtk-FileChooserAction" "save"))

(define FILE_CHOOSER_ACTION_SELECT_FOLDER
  (gi-enum-value "Gtk-FileChooserAction" "select_folder"))

(define FILE_CHOOSER_ACTION_CREATE_FOLDER
  (gi-enum-value "Gtk-FileChooserAction" "create_folder"))

(define <GtkFileChooserButton>
  (gi-lookup-type "Gtk-FileChooserButton"))

(define (GtkFileChooserButton-new title action)
"  ARGS: 
     title  - string, 
     action  - exact integer of enum type FileChooserAction
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileChooserButton-new" title action))

(define (GtkFileChooserButton-new-with-dialog dialog)
"  ARGS: 
     dialog  - object Dialog
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileChooserButton-new_with_dialog" dialog))

(define (GtkFileChooserButton-get-focus-on-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_on_click")))

(define (GtkFileChooserButton-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkFileChooserButton-get-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_width_chars")))

(define (GtkFileChooserButton-set-focus-on-click self focus-on-click)
"  ARGS: 
     focus-on-click  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_on_click" focus-on-click)))

(define (GtkFileChooserButton-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkFileChooserButton-set-width-chars self n-chars)
"  ARGS: 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_width_chars" n-chars)))

(define FILE_CHOOSER_CONFIRMATION_CONFIRM
  (gi-enum-value "Gtk-FileChooserConfirmation" "confirm"))

(define FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME
  (gi-enum-value "Gtk-FileChooserConfirmation" "accept_filename"))

(define FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN
  (gi-enum-value "Gtk-FileChooserConfirmation" "select_again"))

(define <GtkFileChooserDialog>
  (gi-lookup-type "Gtk-FileChooserDialog"))

(define FILE_CHOOSER_ERROR_NONEXISTENT
  (gi-enum-value "Gtk-FileChooserError" "nonexistent"))

(define FILE_CHOOSER_ERROR_BAD_FILENAME
  (gi-enum-value "Gtk-FileChooserError" "bad_filename"))

(define FILE_CHOOSER_ERROR_ALREADY_EXISTS
  (gi-enum-value "Gtk-FileChooserError" "already_exists"))

(define FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME
  (gi-enum-value "Gtk-FileChooserError" "incomplete_hostname"))

(define <GtkFileChooserNative>
  (gi-lookup-type "Gtk-FileChooserNative"))

(define (GtkFileChooserNative-new title parent action accept-label cancel-label)
"  ARGS: 
     title  - #f for NULL or string, 
     parent  - object Window, 
     action  - exact integer of enum type FileChooserAction, 
     accept-label  - #f for NULL or string, 
     cancel-label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileChooserNative-new" title parent action accept-label cancel-label))

(define (GtkFileChooserNative-get-accept-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_accept_label")))

(define (GtkFileChooserNative-get-cancel-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_cancel_label")))

(define (GtkFileChooserNative-set-accept-label self accept-label)
"  ARGS: 
     accept-label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accept_label" accept-label)))

(define (GtkFileChooserNative-set-cancel-label self cancel-label)
"  ARGS: 
     cancel-label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cancel_label" cancel-label)))

(define <GtkFileChooserWidget>
  (gi-lookup-type "Gtk-FileChooserWidget"))

(define (GtkFileChooserWidget-new action)
"  ARGS: 
     action  - exact integer of enum type FileChooserAction
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileChooserWidget-new" action))

(define <GtkFileFilter>
  (gi-lookup-type "Gtk-FileFilter"))

(define (GtkFileFilter-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileFilter-new"))

(define (GtkFileFilter-new-from-gvariant variant)
"  ARGS: 
     variant  - struct Variant
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FileFilter-new_from_gvariant" variant))

(define (GtkFileFilter-add-custom self needed func data notify)
"  ARGS: 
     needed  - exact integer of flags type FileFilterFlags, 
     func  - procedure of type FileFilterFunc, 
     data  - #f for NULL or pointer, 
     notify  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_custom" needed func data notify)))

(define (GtkFileFilter-add-mime-type self mime-type)
"  ARGS: 
     mime-type  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mime_type" mime-type)))

(define (GtkFileFilter-add-pattern self pattern)
"  ARGS: 
     pattern  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_pattern" pattern)))

(define (GtkFileFilter-add-pixbuf-formats self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_pixbuf_formats")))

(define (GtkFileFilter-filter? self filter-info)
"  ARGS: 
     filter-info  - struct FileFilterInfo
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "filter" filter-info)))

(define (GtkFileFilter-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkFileFilter-get-needed self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_needed")))

(define (GtkFileFilter-set-name self name)
"  ARGS: 
     name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_name" name)))

(define (GtkFileFilter-to-gvariant self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "to_gvariant")))

(define FILENAME
  (gi-flag-value "Gtk-FileFilterFlags" "filename"))

(define URI
  (gi-flag-value "Gtk-FileFilterFlags" "uri"))

(define DISPLAY_NAME
  (gi-flag-value "Gtk-FileFilterFlags" "display_name"))

(define MIME_TYPE
  (gi-flag-value "Gtk-FileFilterFlags" "mime_type"))

;; CALLBACK
(define file-filter-func
  (gi-lookup-callback-info "Gtk-FileFilterFunc"))
;; ARGS: 
;;   filter-info  - struct FileFilterInfo, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkFixed>
  (gi-lookup-type "Gtk-Fixed"))

(define (GtkFixed-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Fixed-new"))

(define (GtkFixed-move self widget x y)
"  ARGS: 
     widget  - object Widget, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move" widget x y)))

(define (GtkFixed-put self widget x y)
"  ARGS: 
     widget  - object Widget, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "put" widget x y)))

(define <GtkFlowBox>
  (gi-lookup-type "Gtk-FlowBox"))

(define (GtkFlowBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FlowBox-new"))

(define (GtkFlowBox-bind-model self model create-widget-func user-data user-data-free-func)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16, 
     create-widget-func  - procedure of type FlowBoxCreateWidgetFunc, 
     user-data  - #f for NULL or pointer, 
     user-data-free-func  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "bind_model" model create-widget-func user-data user-data-free-func)))

(define (GtkFlowBox-get-activate-on-single-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activate_on_single_click")))

(define (GtkFlowBox-get-child-at-index self idx)
"  ARGS: 
     idx  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child_at_index" idx)))

(define (GtkFlowBox-get-child-at-pos self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child_at_pos" x y)))

(define (GtkFlowBox-get-column-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_column_spacing")))

(define (GtkFlowBox-get-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_homogeneous")))

(define (GtkFlowBox-get-max-children-per-line self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_children_per_line")))

(define (GtkFlowBox-get-min-children-per-line self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_min_children_per_line")))

(define (GtkFlowBox-get-row-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_row_spacing")))

(define (GtkFlowBox-get-selected-children self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_children")))

(define (GtkFlowBox-get-selection-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_mode")))

(define (GtkFlowBox-insert self widget position)
"  ARGS: 
     widget  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" widget position)))

(define (GtkFlowBox-invalidate-filter self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate_filter")))

(define (GtkFlowBox-invalidate-sort self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate_sort")))

(define (GtkFlowBox-select-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_all")))

(define (GtkFlowBox-select-child self child)
"  ARGS: 
     child  - object FlowBoxChild
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_child" child)))

(define (GtkFlowBox-selected-foreach self func data)
"  ARGS: 
     func  - procedure of type FlowBoxForeachFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "selected_foreach" func data)))

(define (GtkFlowBox-set-activate-on-single-click self single)
"  ARGS: 
     single  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activate_on_single_click" single)))

(define (GtkFlowBox-set-column-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_spacing" spacing)))

(define (GtkFlowBox-set-filter-func self filter-func user-data destroy)
"  ARGS: 
     filter-func  - procedure of type FlowBoxFilterFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_filter_func" filter-func user-data destroy)))

(define (GtkFlowBox-set-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hadjustment" adjustment)))

(define (GtkFlowBox-set-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_homogeneous" homogeneous)))

(define (GtkFlowBox-set-max-children-per-line self n-children)
"  ARGS: 
     n-children  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_children_per_line" n-children)))

(define (GtkFlowBox-set-min-children-per-line self n-children)
"  ARGS: 
     n-children  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_children_per_line" n-children)))

(define (GtkFlowBox-set-row-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_spacing" spacing)))

(define (GtkFlowBox-set-selection-mode self mode)
"  ARGS: 
     mode  - exact integer of enum type SelectionMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_selection_mode" mode)))

(define (GtkFlowBox-set-sort-func self sort-func user-data destroy)
"  ARGS: 
     sort-func  - procedure of type FlowBoxSortFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sort_func" sort-func user-data destroy)))

(define (GtkFlowBox-set-vadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vadjustment" adjustment)))

(define (GtkFlowBox-unselect-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_all")))

(define (GtkFlowBox-unselect-child self child)
"  ARGS: 
     child  - object FlowBoxChild
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_child" child)))

(define <GtkFlowBoxAccessible>
  (gi-lookup-type "Gtk-FlowBoxAccessible"))

(define <GtkFlowBoxChild>
  (gi-lookup-type "Gtk-FlowBoxChild"))

(define (GtkFlowBoxChild-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FlowBoxChild-new"))

(define (GtkFlowBoxChild-changed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "changed")))

(define (GtkFlowBoxChild-get-index self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_index")))

(define (GtkFlowBoxChild-is-selected? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_selected")))

(define <GtkFlowBoxChildAccessible>
  (gi-lookup-type "Gtk-FlowBoxChildAccessible"))

;; CALLBACK
(define flow-box-create-widget-func
  (gi-lookup-callback-info "Gtk-FlowBoxCreateWidgetFunc"))
;; ARGS: 
;;   item  - object Object, 
;;   user-data  - #f for NULL or pointer
;; RETURN: interface*
;; CALLBACK
(define flow-box-filter-func
  (gi-lookup-callback-info "Gtk-FlowBoxFilterFunc"))
;; ARGS: 
;;   child  - object FlowBoxChild, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define flow-box-foreach-func
  (gi-lookup-callback-info "Gtk-FlowBoxForeachFunc"))
;; ARGS: 
;;   box  - object FlowBox, 
;;   child  - object FlowBoxChild, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define flow-box-sort-func
  (gi-lookup-callback-info "Gtk-FlowBoxSortFunc"))
;; ARGS: 
;;   child1  - object FlowBoxChild, 
;;   child2  - object FlowBoxChild, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gint32
(define <GtkFontButton>
  (gi-lookup-type "Gtk-FontButton"))

(define (GtkFontButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontButton-new"))

(define (GtkFontButton-new-with-font fontname)
"  ARGS: 
     fontname  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontButton-new_with_font" fontname))

(define (GtkFontButton-get-font-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_name")))

(define (GtkFontButton-get-show-size? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_size")))

(define (GtkFontButton-get-show-style? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_style")))

(define (GtkFontButton-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkFontButton-get-use-font? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_font")))

(define (GtkFontButton-get-use-size? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_size")))

(define (GtkFontButton-set-font-name? self fontname)
"  ARGS: 
     fontname  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_font_name" fontname)))

(define (GtkFontButton-set-show-size self show-size)
"  ARGS: 
     show-size  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_size" show-size)))

(define (GtkFontButton-set-show-style self show-style)
"  ARGS: 
     show-style  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_style" show-style)))

(define (GtkFontButton-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkFontButton-set-use-font self use-font)
"  ARGS: 
     use-font  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_font" use-font)))

(define (GtkFontButton-set-use-size self use-size)
"  ARGS: 
     use-size  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_size" use-size)))

(define <GtkFontChooserDialog>
  (gi-lookup-type "Gtk-FontChooserDialog"))

(define (GtkFontChooserDialog-new title parent)
"  ARGS: 
     title  - #f for NULL or string, 
     parent  - object Window
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontChooserDialog-new" title parent))

(define FAMILY
  (gi-flag-value "Gtk-FontChooserLevel" "family"))

(define STYLE
  (gi-flag-value "Gtk-FontChooserLevel" "style"))

(define SIZE
  (gi-flag-value "Gtk-FontChooserLevel" "size"))

(define VARIATIONS
  (gi-flag-value "Gtk-FontChooserLevel" "variations"))

(define FEATURES
  (gi-flag-value "Gtk-FontChooserLevel" "features"))

(define <GtkFontChooserWidget>
  (gi-lookup-type "Gtk-FontChooserWidget"))

(define (GtkFontChooserWidget-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontChooserWidget-new"))

;; CALLBACK
(define font-filter-func
  (gi-lookup-callback-info "Gtk-FontFilterFunc"))
;; ARGS: 
;;   family  - object FontFamily, 
;;   face  - object FontFace, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkFontSelection>
  (gi-lookup-type "Gtk-FontSelection"))

(define (GtkFontSelection-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontSelection-new"))

(define (GtkFontSelection-get-face self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_face")))

(define (GtkFontSelection-get-face-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_face_list")))

(define (GtkFontSelection-get-family self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_family")))

(define (GtkFontSelection-get-family-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_family_list")))

(define (GtkFontSelection-get-font-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_name")))

(define (GtkFontSelection-get-preview-entry self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_preview_entry")))

(define (GtkFontSelection-get-preview-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_preview_text")))

(define (GtkFontSelection-get-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkFontSelection-get-size-entry self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_size_entry")))

(define (GtkFontSelection-get-size-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_size_list")))

(define (GtkFontSelection-set-font-name? self fontname)
"  ARGS: 
     fontname  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_font_name" fontname)))

(define (GtkFontSelection-set-preview-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_preview_text" text)))

(define <GtkFontSelectionDialog>
  (gi-lookup-type "Gtk-FontSelectionDialog"))

(define (GtkFontSelectionDialog-new title)
"  ARGS: 
     title  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-FontSelectionDialog-new" title))

(define (GtkFontSelectionDialog-get-cancel-button self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_cancel_button")))

(define (GtkFontSelectionDialog-get-font-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_name")))

(define (GtkFontSelectionDialog-get-font-selection self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_selection")))

(define (GtkFontSelectionDialog-get-ok-button self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_ok_button")))

(define (GtkFontSelectionDialog-get-preview-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_preview_text")))

(define (GtkFontSelectionDialog-set-font-name? self fontname)
"  ARGS: 
     fontname  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_font_name" fontname)))

(define (GtkFontSelectionDialog-set-preview-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_preview_text" text)))

(define <GtkFrame>
  (gi-lookup-type "Gtk-Frame"))

(define (GtkFrame-new label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Frame-new" label))

(define (GtkFrame-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkFrame-get-label-align self)
"  ARGS: 
     xalign  - real number of size gfloat[OUT], 
     yalign  - real number of size gfloat[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_label_align")))

(define (GtkFrame-get-label-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_label_widget")))

(define (GtkFrame-get-shadow-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_shadow_type")))

(define (GtkFrame-set-label self label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkFrame-set-label-align self xalign yalign)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_align" xalign yalign)))

(define (GtkFrame-set-label-widget self label-widget)
"  ARGS: 
     label-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_widget" label-widget)))

(define (GtkFrame-set-shadow-type self type)
"  ARGS: 
     type  - exact integer of enum type ShadowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_shadow_type" type)))

(define <GtkFrameAccessible>
  (gi-lookup-type "Gtk-FrameAccessible"))

(define <GtkGLArea>
  (gi-lookup-type "Gtk-GLArea"))

(define (GtkGLArea-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GLArea-new"))

(define (GtkGLArea-attach-buffers self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach_buffers")))

(define (GtkGLArea-get-auto-render? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_auto_render")))

(define (GtkGLArea-get-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_context")))

(define (GtkGLArea-get-error self)
"  ARGS: 
   RETURN: error*
"
  (gi-method-send self 
     (gi-method-prepare "get_error")))

(define (GtkGLArea-get-has-alpha? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_alpha")))

(define (GtkGLArea-get-has-depth-buffer? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_depth_buffer")))

(define (GtkGLArea-get-has-stencil-buffer? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_stencil_buffer")))

(define (GtkGLArea-get-required-version self)
"  ARGS: 
     major  - exact integer of size gint32[OUT], 
     minor  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_required_version")))

(define (GtkGLArea-get-use-es? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_es")))

(define (GtkGLArea-make-current self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "make_current")))

(define (GtkGLArea-queue-render self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_render")))

(define (GtkGLArea-set-auto-render self auto-render)
"  ARGS: 
     auto-render  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_auto_render" auto-render)))

(define (GtkGLArea-set-error self error)
"  ARGS: 
     error  - #f for NULL or Unhandled argument type tag 20
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_error" error)))

(define (GtkGLArea-set-has-alpha self has-alpha)
"  ARGS: 
     has-alpha  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_alpha" has-alpha)))

(define (GtkGLArea-set-has-depth-buffer self has-depth-buffer)
"  ARGS: 
     has-depth-buffer  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_depth_buffer" has-depth-buffer)))

(define (GtkGLArea-set-has-stencil-buffer self has-stencil-buffer)
"  ARGS: 
     has-stencil-buffer  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_stencil_buffer" has-stencil-buffer)))

(define (GtkGLArea-set-required-version self major minor)
"  ARGS: 
     major  - exact integer of size gint32, 
     minor  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_required_version" major minor)))

(define (GtkGLArea-set-use-es self use-es)
"  ARGS: 
     use-es  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_es" use-es)))

(define <GtkGesture>
  (gi-lookup-type "Gtk-Gesture"))

(define (GtkGesture-get-bounding-box? self out-rect)
"  ARGS: 
   RETURN: gboolean
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_bounding_box" out-rect)))

(define (GtkGesture-get-bounding-box-center? self)
"  ARGS: 
     x  - real number of size gdouble[OUT], 
     y  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_bounding_box_center")))

(define (GtkGesture-get-device self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_device")))

(define (GtkGesture-get-group self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkGesture-get-last-event self sequence)
"  ARGS: 
     sequence  - struct EventSequence
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_last_event" sequence)))

(define (GtkGesture-get-last-updated-sequence self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_last_updated_sequence")))

(define (GtkGesture-get-point? self sequence)
"  ARGS: 
     sequence  - struct EventSequence, 
     x  - real number of size gdouble[OUT], 
     y  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_point" sequence)))

(define (GtkGesture-get-sequence-state self sequence)
"  ARGS: 
     sequence  - struct EventSequence
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_sequence_state" sequence)))

(define (GtkGesture-get-sequences self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_sequences")))

(define (GtkGesture-get-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_window")))

(define (GtkGesture-group self gesture)
"  ARGS: 
     gesture  - object Gesture
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "group" gesture)))

(define (GtkGesture-handles-sequence? self sequence)
"  ARGS: 
     sequence  - struct EventSequence
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "handles_sequence" sequence)))

(define (GtkGesture-is-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_active")))

(define (GtkGesture-is-grouped-with? self other)
"  ARGS: 
     other  - object Gesture
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_grouped_with" other)))

(define (GtkGesture-is-recognized? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_recognized")))

(define (GtkGesture-set-sequence-state? self sequence state)
"  ARGS: 
     sequence  - struct EventSequence, 
     state  - exact integer of enum type EventSequenceState
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_sequence_state" sequence state)))

(define (GtkGesture-set-state? self state)
"  ARGS: 
     state  - exact integer of enum type EventSequenceState
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_state" state)))

(define (GtkGesture-set-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_window" window)))

(define (GtkGesture-ungroup self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "ungroup")))

(define <GtkGestureDrag>
  (gi-lookup-type "Gtk-GestureDrag"))

(define (GtkGestureDrag-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureDrag-new" widget))

(define (GtkGestureDrag-get-offset? self)
"  ARGS: 
     x  - #f for NULL or real number of size gdouble[OUT], 
     y  - #f for NULL or real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_offset")))

(define (GtkGestureDrag-get-start-point? self)
"  ARGS: 
     x  - #f for NULL or real number of size gdouble[OUT], 
     y  - #f for NULL or real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_start_point")))

(define <GtkGestureLongPress>
  (gi-lookup-type "Gtk-GestureLongPress"))

(define (GtkGestureLongPress-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureLongPress-new" widget))

(define <GtkGestureMultiPress>
  (gi-lookup-type "Gtk-GestureMultiPress"))

(define (GtkGestureMultiPress-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureMultiPress-new" widget))

(define (GtkGestureMultiPress-get-area? self out-rect)
"  ARGS: 
   RETURN: gboolean
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_area" out-rect)))

(define (GtkGestureMultiPress-set-area self rect)
"  ARGS: 
     rect  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_area" rect)))

(define <GtkGesturePan>
  (gi-lookup-type "Gtk-GesturePan"))

(define (GtkGesturePan-new widget orientation)
"  ARGS: 
     widget  - object Widget, 
     orientation  - exact integer of enum type Orientation
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GesturePan-new" widget orientation))

(define (GtkGesturePan-get-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_orientation")))

(define (GtkGesturePan-set-orientation self orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_orientation" orientation)))

(define <GtkGestureRotate>
  (gi-lookup-type "Gtk-GestureRotate"))

(define (GtkGestureRotate-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureRotate-new" widget))

(define (GtkGestureRotate-get-angle-delta self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_angle_delta")))

(define <GtkGestureSingle>
  (gi-lookup-type "Gtk-GestureSingle"))

(define (GtkGestureSingle-get-button self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_button")))

(define (GtkGestureSingle-get-current-button self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_current_button")))

(define (GtkGestureSingle-get-current-sequence self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_current_sequence")))

(define (GtkGestureSingle-get-exclusive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_exclusive")))

(define (GtkGestureSingle-get-touch-only? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_touch_only")))

(define (GtkGestureSingle-set-button self button)
"  ARGS: 
     button  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_button" button)))

(define (GtkGestureSingle-set-exclusive self exclusive)
"  ARGS: 
     exclusive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_exclusive" exclusive)))

(define (GtkGestureSingle-set-touch-only self touch-only)
"  ARGS: 
     touch-only  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_touch_only" touch-only)))

(define <GtkGestureStylus>
  (gi-lookup-type "Gtk-GestureStylus"))

(define (GtkGestureStylus-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureStylus-new" widget))

(define (GtkGestureStylus-get-axes? self axes)
"  ARGS: 
     axes  - Unhandled argument type tag 15, 
     values  - Unhandled argument type tag 15[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_axes" axes)))

(define (GtkGestureStylus-get-axis? self axis)
"  ARGS: 
     axis  - exact integer of enum type AxisUse, 
     value  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_axis" axis)))

(define (GtkGestureStylus-get-device-tool self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_device_tool")))

(define <GtkGestureSwipe>
  (gi-lookup-type "Gtk-GestureSwipe"))

(define (GtkGestureSwipe-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureSwipe-new" widget))

(define (GtkGestureSwipe-get-velocity? self)
"  ARGS: 
     velocity-x  - real number of size gdouble[OUT], 
     velocity-y  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_velocity")))

(define <GtkGestureZoom>
  (gi-lookup-type "Gtk-GestureZoom"))

(define (GtkGestureZoom-new widget)
"  ARGS: 
     widget  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-GestureZoom-new" widget))

(define (GtkGestureZoom-get-scale-delta self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_scale_delta")))

(define <GtkGradient>
  (gi-lookup-type "Gtk-Gradient"))

(define (GtkGradient-new-linear x0 y0 x1 y1)
"  ARGS: 
     x0  - real number of size gdouble, 
     y0  - real number of size gdouble, 
     x1  - real number of size gdouble, 
     y1  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Gradient-new_linear" x0 y0 x1 y1))

(define (GtkGradient-new-radial x0 y0 radius0 x1 y1 radius1)
"  ARGS: 
     x0  - real number of size gdouble, 
     y0  - real number of size gdouble, 
     radius0  - real number of size gdouble, 
     x1  - real number of size gdouble, 
     y1  - real number of size gdouble, 
     radius1  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Gradient-new_radial" x0 y0 radius0 x1 y1 radius1))

(define (GtkGradient-add-color-stop self offset color)
"  ARGS: 
     offset  - real number of size gdouble, 
     color  - struct SymbolicColor
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_color_stop" offset color)))

(define (GtkGradient-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkGradient-resolve? self props)
"  ARGS: 
     props  - object StyleProperties, 
     resolved-gradient  - struct Pattern[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "resolve" props)))

(define (GtkGradient-resolve-for-context self context)
"  ARGS: 
     context  - object StyleContext
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "resolve_for_context" context)))

(define (GtkGradient-to-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string")))

(define (GtkGradient-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define <GtkGrid>
  (gi-lookup-type "Gtk-Grid"))

(define (GtkGrid-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Grid-new"))

(define (GtkGrid-attach self child left top width height)
"  ARGS: 
     child  - object Widget, 
     left  - exact integer of size gint32, 
     top  - exact integer of size gint32, 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach" child left top width height)))

(define (GtkGrid-attach-next-to self child sibling side width height)
"  ARGS: 
     child  - object Widget, 
     sibling  - object Widget, 
     side  - exact integer of enum type PositionType, 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach_next_to" child sibling side width height)))

(define (GtkGrid-get-baseline-row self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_baseline_row")))

(define (GtkGrid-get-child-at self left top)
"  ARGS: 
     left  - exact integer of size gint32, 
     top  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child_at" left top)))

(define (GtkGrid-get-column-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_column_homogeneous")))

(define (GtkGrid-get-column-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_column_spacing")))

(define (GtkGrid-get-row-baseline-position self row)
"  ARGS: 
     row  - exact integer of size gint32
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_row_baseline_position" row)))

(define (GtkGrid-get-row-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_row_homogeneous")))

(define (GtkGrid-get-row-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_row_spacing")))

(define (GtkGrid-insert-column self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_column" position)))

(define (GtkGrid-insert-next-to self sibling side)
"  ARGS: 
     sibling  - object Widget, 
     side  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_next_to" sibling side)))

(define (GtkGrid-insert-row self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_row" position)))

(define (GtkGrid-remove-column self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_column" position)))

(define (GtkGrid-remove-row self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_row" position)))

(define (GtkGrid-set-baseline-row self row)
"  ARGS: 
     row  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_baseline_row" row)))

(define (GtkGrid-set-column-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_homogeneous" homogeneous)))

(define (GtkGrid-set-column-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_spacing" spacing)))

(define (GtkGrid-set-row-baseline-position self row pos)
"  ARGS: 
     row  - exact integer of size gint32, 
     pos  - exact integer of enum type BaselinePosition
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_baseline_position" row pos)))

(define (GtkGrid-set-row-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_homogeneous" homogeneous)))

(define (GtkGrid-set-row-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_spacing" spacing)))

(define <GtkHBox>
  (gi-lookup-type "Gtk-HBox"))

(define (GtkHBox-new homogeneous spacing)
"  ARGS: 
     homogeneous  - boolean, 
     spacing  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HBox-new" homogeneous spacing))

(define <GtkHButtonBox>
  (gi-lookup-type "Gtk-HButtonBox"))

(define (GtkHButtonBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HButtonBox-new"))

(define <GtkHPaned>
  (gi-lookup-type "Gtk-HPaned"))

(define (GtkHPaned-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HPaned-new"))

(define <GtkHSV>
  (gi-lookup-type "Gtk-HSV"))

(define (GtkHSV-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HSV-new"))

(define (GtkHSV-to-rgb h s v)
"  ARGS: 
     h  - real number of size gdouble, 
     s  - real number of size gdouble, 
     v  - real number of size gdouble, 
     r  - real number of size gdouble[OUT], 
     g  - real number of size gdouble[OUT], 
     b  - real number of size gdouble[OUT]
   RETURN: void
"
  (gi-function-invoke "Gtk-HSV-to_rgb" h s v))

(define (GtkHSV-get-color self)
"  ARGS: 
     h  - real number of size gdouble[OUT], 
     s  - real number of size gdouble[OUT], 
     v  - real number of size gdouble[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_color")))

(define (GtkHSV-get-metrics self)
"  ARGS: 
     size  - exact integer of size gint32[OUT], 
     ring-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_metrics")))

(define (GtkHSV-is-adjusting? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_adjusting")))

(define (GtkHSV-set-color self h s v)
"  ARGS: 
     h  - real number of size gdouble, 
     s  - real number of size gdouble, 
     v  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_color" h s v)))

(define (GtkHSV-set-metrics self size ring-width)
"  ARGS: 
     size  - exact integer of size gint32, 
     ring-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_metrics" size ring-width)))

(define <GtkHScale>
  (gi-lookup-type "Gtk-HScale"))

(define (GtkHScale-new adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HScale-new" adjustment))

(define (GtkHScale-new-with-range min max step)
"  ARGS: 
     min  - real number of size gdouble, 
     max  - real number of size gdouble, 
     step  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HScale-new_with_range" min max step))

(define <GtkHScrollbar>
  (gi-lookup-type "Gtk-HScrollbar"))

(define (GtkHScrollbar-new adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HScrollbar-new" adjustment))

(define <GtkHSeparator>
  (gi-lookup-type "Gtk-HSeparator"))

(define (GtkHSeparator-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HSeparator-new"))

(define <GtkHandleBox>
  (gi-lookup-type "Gtk-HandleBox"))

(define (GtkHandleBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HandleBox-new"))

(define (GtkHandleBox-get-child-detached? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_child_detached")))

(define (GtkHandleBox-get-handle-position self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_handle_position")))

(define (GtkHandleBox-get-shadow-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_shadow_type")))

(define (GtkHandleBox-get-snap-edge self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_snap_edge")))

(define (GtkHandleBox-set-handle-position self position)
"  ARGS: 
     position  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_handle_position" position)))

(define (GtkHandleBox-set-shadow-type self type)
"  ARGS: 
     type  - exact integer of enum type ShadowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_shadow_type" type)))

(define (GtkHandleBox-set-snap-edge self edge)
"  ARGS: 
     edge  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_snap_edge" edge)))

(define <GtkHeaderBar>
  (gi-lookup-type "Gtk-HeaderBar"))

(define (GtkHeaderBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-HeaderBar-new"))

(define (GtkHeaderBar-get-custom-title self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_custom_title")))

(define (GtkHeaderBar-get-decoration-layout self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_decoration_layout")))

(define (GtkHeaderBar-get-has-subtitle? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_subtitle")))

(define (GtkHeaderBar-get-show-close-button? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_close_button")))

(define (GtkHeaderBar-get-subtitle self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_subtitle")))

(define (GtkHeaderBar-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkHeaderBar-pack-end self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_end" child)))

(define (GtkHeaderBar-pack-start self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_start" child)))

(define (GtkHeaderBar-set-custom-title self title-widget)
"  ARGS: 
     title-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_custom_title" title-widget)))

(define (GtkHeaderBar-set-decoration-layout self layout)
"  ARGS: 
     layout  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_decoration_layout" layout)))

(define (GtkHeaderBar-set-has-subtitle self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_subtitle" setting)))

(define (GtkHeaderBar-set-show-close-button self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_close_button" setting)))

(define (GtkHeaderBar-set-subtitle self subtitle)
"  ARGS: 
     subtitle  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_subtitle" subtitle)))

(define (GtkHeaderBar-set-title self title)
"  ARGS: 
     title  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define <GtkIMContext>
  (gi-lookup-type "Gtk-IMContext"))

(define (GtkIMContext-delete-surrounding? self offset n-chars)
"  ARGS: 
     offset  - exact integer of size gint32, 
     n-chars  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "delete_surrounding" offset n-chars)))

(define (GtkIMContext-filter-keypress? self event)
"  ARGS: 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "filter_keypress" event)))

(define (GtkIMContext-focus-in self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "focus_in")))

(define (GtkIMContext-focus-out self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "focus_out")))

(define (GtkIMContext-get-preedit-string self)
"  ARGS: 
     str  - string[OUT], 
     attrs  - struct AttrList[OUT], 
     cursor-pos  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preedit_string")))

(define (GtkIMContext-get-surrounding? self)
"  ARGS: 
     text  - string[OUT], 
     cursor-index  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_surrounding")))

(define (GtkIMContext-reset self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset")))

(define (GtkIMContext-set-client-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_client_window" window)))

(define (GtkIMContext-set-cursor-location self area)
"  ARGS: 
     area  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor_location" area)))

(define (GtkIMContext-set-surrounding self text len cursor-index)
"  ARGS: 
     text  - string, 
     len  - exact integer of size gint32, 
     cursor-index  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_surrounding" text len cursor-index)))

(define (GtkIMContext-set-use-preedit self use-preedit)
"  ARGS: 
     use-preedit  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_preedit" use-preedit)))

(define <GtkIMContextSimple>
  (gi-lookup-type "Gtk-IMContextSimple"))

(define (GtkIMContextSimple-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IMContextSimple-new"))

(define (GtkIMContextSimple-add-compose-file self compose-file)
"  ARGS: 
     compose-file  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_compose_file" compose-file)))

(define <GtkIMMulticontext>
  (gi-lookup-type "Gtk-IMMulticontext"))

(define (GtkIMMulticontext-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IMMulticontext-new"))

(define (GtkIMMulticontext-append-menuitems self menushell)
"  ARGS: 
     menushell  - object MenuShell
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_menuitems" menushell)))

(define (GtkIMMulticontext-get-context-id self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_context_id")))

(define (GtkIMMulticontext-set-context-id self context-id)
"  ARGS: 
     context-id  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_context_id" context-id)))

(define GTK_INPUT_ERROR
  (gi-constant-value "Gtk-INPUT_ERROR"))

(define GTK_INTERFACE_AGE
  (gi-constant-value "Gtk-INTERFACE_AGE"))

(define <GtkIconFactory>
  (gi-lookup-type "Gtk-IconFactory"))

(define (GtkIconFactory-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconFactory-new"))

(define (GtkIconFactory-lookup-default stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconFactory-lookup_default" stock-id))

(define (GtkIconFactory-add self stock-id icon-set)
"  ARGS: 
     stock-id  - string, 
     icon-set  - struct IconSet
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add" stock-id icon-set)))

(define (GtkIconFactory-add-default self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_default")))

(define (GtkIconFactory-lookup self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup" stock-id)))

(define (GtkIconFactory-remove-default self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_default")))

(define <GtkIconInfo>
  (gi-lookup-type "Gtk-IconInfo"))

(define (GtkIconInfo-new-for-pixbuf icon-theme pixbuf)
"  ARGS: 
     icon-theme  - object IconTheme, 
     pixbuf  - object Pixbuf
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconInfo-new_for_pixbuf" icon-theme pixbuf))

(define (GtkIconInfo-get-attach-points? self)
"  ARGS: 
     points  - Unhandled argument type tag 15[OUT], 
     n-points  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_attach_points")))

(define (GtkIconInfo-get-base-scale self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_base_scale")))

(define (GtkIconInfo-get-base-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_base_size")))

(define (GtkIconInfo-get-builtin-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_builtin_pixbuf")))

(define (GtkIconInfo-get-display-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_display_name")))

(define (GtkIconInfo-get-embedded-rect? self out-rectangle)
"  ARGS: 
   RETURN: gboolean
     rectangle  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_embedded_rect" out-rectangle)))

(define (GtkIconInfo-get-filename self)
"  ARGS: 
   RETURN: filename*
"
  (gi-method-send self 
     (gi-method-prepare "get_filename")))

(define (GtkIconInfo-is-symbolic? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_symbolic")))

(define (GtkIconInfo-load-icon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_icon")))

(define (GtkIconInfo-load-icon-async self cancellable callback user-data)
"  ARGS: 
     cancellable  - object Cancellable, 
     callback  - procedure of type AsyncReadyCallback, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "load_icon_async" cancellable callback user-data)))

(define (GtkIconInfo-load-icon-finish self res)
"  ARGS: 
     res  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_icon_finish" res)))

(define (GtkIconInfo-load-surface self for-window)
"  ARGS: 
     for-window  - object Window
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_surface" for-window)))

(define (GtkIconInfo-load-symbolic self fg success-color warning-color error-color)
"  ARGS: 
     fg  - struct RGBA, 
     success-color  - struct RGBA, 
     warning-color  - struct RGBA, 
     error-color  - struct RGBA, 
     was-symbolic  - boolean[OUT]
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic" fg success-color warning-color error-color)))

(define (GtkIconInfo-load-symbolic-async self fg success-color warning-color error-color cancellable callback user-data)
"  ARGS: 
     fg  - struct RGBA, 
     success-color  - struct RGBA, 
     warning-color  - struct RGBA, 
     error-color  - struct RGBA, 
     cancellable  - object Cancellable, 
     callback  - procedure of type AsyncReadyCallback, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_async" fg success-color warning-color error-color cancellable callback user-data)))

(define (GtkIconInfo-load-symbolic-finish self res)
"  ARGS: 
     res  - Unhandled argument type tag 16, 
     was-symbolic  - boolean[OUT]
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_finish" res)))

(define (GtkIconInfo-load-symbolic-for-context self context)
"  ARGS: 
     context  - object StyleContext, 
     was-symbolic  - boolean[OUT]
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_for_context" context)))

(define (GtkIconInfo-load-symbolic-for-context-async self context cancellable callback user-data)
"  ARGS: 
     context  - object StyleContext, 
     cancellable  - object Cancellable, 
     callback  - procedure of type AsyncReadyCallback, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_for_context_async" context cancellable callback user-data)))

(define (GtkIconInfo-load-symbolic-for-context-finish self res)
"  ARGS: 
     res  - Unhandled argument type tag 16, 
     was-symbolic  - boolean[OUT]
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_for_context_finish" res)))

(define (GtkIconInfo-load-symbolic-for-style self style state)
"  ARGS: 
     style  - object Style, 
     state  - exact integer of enum type StateType, 
     was-symbolic  - boolean[OUT]
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_symbolic_for_style" style state)))

(define (GtkIconInfo-set-raw-coordinates self raw-coordinates)
"  ARGS: 
     raw-coordinates  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_raw_coordinates" raw-coordinates)))

(define NO_SVG
  (gi-flag-value "Gtk-IconLookupFlags" "no_svg"))

(define FORCE_SVG
  (gi-flag-value "Gtk-IconLookupFlags" "force_svg"))

(define USE_BUILTIN
  (gi-flag-value "Gtk-IconLookupFlags" "use_builtin"))

(define GENERIC_FALLBACK
  (gi-flag-value "Gtk-IconLookupFlags" "generic_fallback"))

(define FORCE_SIZE
  (gi-flag-value "Gtk-IconLookupFlags" "force_size"))

(define FORCE_REGULAR
  (gi-flag-value "Gtk-IconLookupFlags" "force_regular"))

(define FORCE_SYMBOLIC
  (gi-flag-value "Gtk-IconLookupFlags" "force_symbolic"))

(define DIR_LTR
  (gi-flag-value "Gtk-IconLookupFlags" "dir_ltr"))

(define DIR_RTL
  (gi-flag-value "Gtk-IconLookupFlags" "dir_rtl"))

(define <GtkIconSet>
  (gi-lookup-type "Gtk-IconSet"))

(define (GtkIconSet-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconSet-new"))

(define (GtkIconSet-new-from-pixbuf pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconSet-new_from_pixbuf" pixbuf))

(define (GtkIconSet-add-source self source)
"  ARGS: 
     source  - struct IconSource
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_source" source)))

(define (GtkIconSet-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkIconSet-get-sizes self)
"  ARGS: 
     sizes  - Unhandled argument type tag 15[OUT], 
     n-sizes  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_sizes")))

(define (GtkIconSet-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkIconSet-render-icon self style direction state size widget detail)
"  ARGS: 
     style  - object Style, 
     direction  - exact integer of enum type TextDirection, 
     state  - exact integer of enum type StateType, 
     size  - exact integer of size gint32, 
     widget  - object Widget, 
     detail  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon" style direction state size widget detail)))

(define (GtkIconSet-render-icon-pixbuf self context size)
"  ARGS: 
     context  - object StyleContext, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon_pixbuf" context size)))

(define (GtkIconSet-render-icon-surface self context size scale for-window)
"  ARGS: 
     context  - object StyleContext, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     for-window  - object Window
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon_surface" context size scale for-window)))

(define (GtkIconSet-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define ICON_SIZE_INVALID
  (gi-enum-value "Gtk-IconSize" "invalid"))

(define ICON_SIZE_MENU
  (gi-enum-value "Gtk-IconSize" "menu"))

(define ICON_SIZE_SMALL_TOOLBAR
  (gi-enum-value "Gtk-IconSize" "small_toolbar"))

(define ICON_SIZE_LARGE_TOOLBAR
  (gi-enum-value "Gtk-IconSize" "large_toolbar"))

(define ICON_SIZE_BUTTON
  (gi-enum-value "Gtk-IconSize" "button"))

(define ICON_SIZE_DND
  (gi-enum-value "Gtk-IconSize" "dnd"))

(define ICON_SIZE_DIALOG
  (gi-enum-value "Gtk-IconSize" "dialog"))

(define <GtkIconSource>
  (gi-lookup-type "Gtk-IconSource"))

(define (GtkIconSource-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconSource-new"))

(define (GtkIconSource-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkIconSource-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkIconSource-get-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_direction")))

(define (GtkIconSource-get-direction-wildcarded? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_direction_wildcarded")))

(define (GtkIconSource-get-filename self)
"  ARGS: 
   RETURN: filename*
"
  (gi-method-send self 
     (gi-method-prepare "get_filename")))

(define (GtkIconSource-get-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkIconSource-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkIconSource-get-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkIconSource-get-size-wildcarded? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_size_wildcarded")))

(define (GtkIconSource-get-state self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state")))

(define (GtkIconSource-get-state-wildcarded? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_state_wildcarded")))

(define (GtkIconSource-set-direction self direction)
"  ARGS: 
     direction  - exact integer of enum type TextDirection
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_direction" direction)))

(define (GtkIconSource-set-direction-wildcarded self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_direction_wildcarded" setting)))

(define (GtkIconSource-set-filename self filename)
"  ARGS: 
     filename  - locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_filename" filename)))

(define (GtkIconSource-set-icon-name self icon-name)
"  ARGS: 
     icon-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_name" icon-name)))

(define (GtkIconSource-set-pixbuf self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixbuf" pixbuf)))

(define (GtkIconSource-set-size self size)
"  ARGS: 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_size" size)))

(define (GtkIconSource-set-size-wildcarded self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_size_wildcarded" setting)))

(define (GtkIconSource-set-state self state)
"  ARGS: 
     state  - exact integer of enum type StateType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state" state)))

(define (GtkIconSource-set-state-wildcarded self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state_wildcarded" setting)))

(define <GtkIconTheme>
  (gi-lookup-type "Gtk-IconTheme"))

(define (GtkIconTheme-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconTheme-new"))

(define (GtkIconTheme-add-builtin-icon icon-name size pixbuf)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-function-invoke "Gtk-IconTheme-add_builtin_icon" icon-name size pixbuf))

(define (GtkIconTheme-get-default)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconTheme-get_default"))

(define (GtkIconTheme-get-for-screen screen)
"  ARGS: 
     screen  - object Screen
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconTheme-get_for_screen" screen))

(define (GtkIconTheme-add-resource-path self path)
"  ARGS: 
     path  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_resource_path" path)))

(define (GtkIconTheme-append-search-path self path)
"  ARGS: 
     path  - locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_search_path" path)))

(define (GtkIconTheme-choose-icon self icon-names size flags)
"  ARGS: 
     icon-names  - Unhandled argument type tag 15, 
     size  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "choose_icon" icon-names size flags)))

(define (GtkIconTheme-choose-icon-for-scale self icon-names size scale flags)
"  ARGS: 
     icon-names  - Unhandled argument type tag 15, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "choose_icon_for_scale" icon-names size scale flags)))

(define (GtkIconTheme-get-example-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_example_icon_name")))

(define (GtkIconTheme-get-icon-sizes self icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_sizes" icon-name)))

(define (GtkIconTheme-get-search-path self)
"  ARGS: 
     path  - Unhandled argument type tag 15[OUT], 
     n-elements  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_search_path")))

(define (GtkIconTheme-has-icon? self icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_icon" icon-name)))

(define (GtkIconTheme-list-contexts self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_contexts")))

(define (GtkIconTheme-list-icons self context)
"  ARGS: 
     context  - #f for NULL or string
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_icons" context)))

(define (GtkIconTheme-load-icon self icon-name size flags)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_icon" icon-name size flags)))

(define (GtkIconTheme-load-icon-for-scale self icon-name size scale flags)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_icon_for_scale" icon-name size scale flags)))

(define (GtkIconTheme-load-surface self icon-name size scale for-window flags)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     for-window  - object Window, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "load_surface" icon-name size scale for-window flags)))

(define (GtkIconTheme-lookup-by-gicon self icon size flags)
"  ARGS: 
     icon  - Unhandled argument type tag 16, 
     size  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_by_gicon" icon size flags)))

(define (GtkIconTheme-lookup-by-gicon-for-scale self icon size scale flags)
"  ARGS: 
     icon  - Unhandled argument type tag 16, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_by_gicon_for_scale" icon size scale flags)))

(define (GtkIconTheme-lookup-icon self icon-name size flags)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_icon" icon-name size flags)))

(define (GtkIconTheme-lookup-icon-for-scale self icon-name size scale flags)
"  ARGS: 
     icon-name  - string, 
     size  - exact integer of size gint32, 
     scale  - exact integer of size gint32, 
     flags  - exact integer of flags type IconLookupFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_icon_for_scale" icon-name size scale flags)))

(define (GtkIconTheme-prepend-search-path self path)
"  ARGS: 
     path  - locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend_search_path" path)))

(define (GtkIconTheme-rescan-if-needed? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "rescan_if_needed")))

(define (GtkIconTheme-set-custom-theme self theme-name)
"  ARGS: 
     theme-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_custom_theme" theme-name)))

(define (GtkIconTheme-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define (GtkIconTheme-set-search-path self path n-elements)
"  ARGS: 
     path  - Unhandled argument type tag 15, 
     n-elements  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_path" path n-elements)))

(define ICON_THEME_ERROR_NOT_FOUND
  (gi-enum-value "Gtk-IconThemeError" "not_found"))

(define ICON_THEME_ERROR_FAILED
  (gi-enum-value "Gtk-IconThemeError" "failed"))

(define <GtkIconView>
  (gi-lookup-type "Gtk-IconView"))

(define (GtkIconView-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconView-new"))

(define (GtkIconView-new-with-area area)
"  ARGS: 
     area  - object CellArea
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconView-new_with_area" area))

(define (GtkIconView-new-with-model model)
"  ARGS: 
     model  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-IconView-new_with_model" model))

(define (GtkIconView-convert-widget-to-bin-window-coords self wx wy)
"  ARGS: 
     wx  - exact integer of size gint32, 
     wy  - exact integer of size gint32, 
     bx  - exact integer of size gint32[OUT], 
     by  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_widget_to_bin_window_coords" wx wy)))

(define (GtkIconView-create-drag-icon self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_drag_icon" path)))

(define (GtkIconView-enable-model-drag-dest self targets n-targets actions)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "enable_model_drag_dest" targets n-targets actions)))

(define (GtkIconView-enable-model-drag-source self start-button-mask targets n-targets actions)
"  ARGS: 
     start-button-mask  - exact integer of flags type ModifierType, 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "enable_model_drag_source" start-button-mask targets n-targets actions)))

(define (GtkIconView-get-activate-on-single-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activate_on_single_click")))

(define (GtkIconView-get-cell-rect? self path cell out-rect)
"  ARGS: 
     path  - struct TreePath, 
     cell  - object CellRenderer, 
   RETURN: gboolean
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_cell_rect" path cell out-rect)))

(define (GtkIconView-get-column-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_column_spacing")))

(define (GtkIconView-get-columns self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_columns")))

(define (GtkIconView-get-cursor? self)
"  ARGS: 
     path  - struct TreePath[OUT], 
     cell  - object CellRenderer[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_cursor")))

(define (GtkIconView-get-dest-item-at-pos? self drag-x drag-y)
"  ARGS: 
     drag-x  - exact integer of size gint32, 
     drag-y  - exact integer of size gint32, 
     path  - struct TreePath[OUT], 
     pos  - exact integer of enum type IconViewDropPosition[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_dest_item_at_pos" drag-x drag-y)))

(define (GtkIconView-get-drag-dest-item self)
"  ARGS: 
     path  - struct TreePath[OUT], 
     pos  - exact integer of enum type IconViewDropPosition[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_drag_dest_item")))

(define (GtkIconView-get-item-at-pos? self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
     path  - struct TreePath[OUT], 
     cell  - object CellRenderer[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_item_at_pos" x y)))

(define (GtkIconView-get-item-column self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_column" path)))

(define (GtkIconView-get-item-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_item_orientation")))

(define (GtkIconView-get-item-padding self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_padding")))

(define (GtkIconView-get-item-row self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_row" path)))

(define (GtkIconView-get-item-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_width")))

(define (GtkIconView-get-margin self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin")))

(define (GtkIconView-get-markup-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_markup_column")))

(define (GtkIconView-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkIconView-get-path-at-pos self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path_at_pos" x y)))

(define (GtkIconView-get-pixbuf-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf_column")))

(define (GtkIconView-get-reorderable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reorderable")))

(define (GtkIconView-get-row-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_row_spacing")))

(define (GtkIconView-get-selected-items self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_items")))

(define (GtkIconView-get-selection-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_mode")))

(define (GtkIconView-get-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_spacing")))

(define (GtkIconView-get-text-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_text_column")))

(define (GtkIconView-get-tooltip-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_column")))

(define (GtkIconView-get-tooltip-context? self x y keyboard-tip out-iter)
"  ARGS: 
     x  - exact integer of size gint32[INOUT] , 
     y  - exact integer of size gint32[INOUT] , 
     keyboard-tip  - boolean, 
     model  - Unhandled argument type tag 16[OUT], 
     path  - struct TreePath[OUT], 
   RETURN: gboolean
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_context" x y keyboard-tip out-iter)))

(define (GtkIconView-get-visible-range? self)
"  ARGS: 
     start-path  - struct TreePath[OUT], 
     end-path  - struct TreePath[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_range")))

(define (GtkIconView-item-activated self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "item_activated" path)))

(define (GtkIconView-path-is-selected? self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "path_is_selected" path)))

(define (GtkIconView-scroll-to-path self path use-align row-align col-align)
"  ARGS: 
     path  - struct TreePath, 
     use-align  - boolean, 
     row-align  - real number of size gfloat, 
     col-align  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_to_path" path use-align row-align col-align)))

(define (GtkIconView-select-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_all")))

(define (GtkIconView-select-path self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_path" path)))

(define (GtkIconView-selected-foreach self func data)
"  ARGS: 
     func  - procedure of type IconViewForeachFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "selected_foreach" func data)))

(define (GtkIconView-set-activate-on-single-click self single)
"  ARGS: 
     single  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activate_on_single_click" single)))

(define (GtkIconView-set-column-spacing self column-spacing)
"  ARGS: 
     column-spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_spacing" column-spacing)))

(define (GtkIconView-set-columns self columns)
"  ARGS: 
     columns  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_columns" columns)))

(define (GtkIconView-set-cursor self path cell start-editing)
"  ARGS: 
     path  - struct TreePath, 
     cell  - object CellRenderer, 
     start-editing  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor" path cell start-editing)))

(define (GtkIconView-set-drag-dest-item self path pos)
"  ARGS: 
     path  - struct TreePath, 
     pos  - exact integer of enum type IconViewDropPosition
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_drag_dest_item" path pos)))

(define (GtkIconView-set-item-orientation self orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_item_orientation" orientation)))

(define (GtkIconView-set-item-padding self item-padding)
"  ARGS: 
     item-padding  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_item_padding" item-padding)))

(define (GtkIconView-set-item-width self item-width)
"  ARGS: 
     item-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_item_width" item-width)))

(define (GtkIconView-set-margin self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin" margin)))

(define (GtkIconView-set-markup-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_markup_column" column)))

(define (GtkIconView-set-model self model)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_model" model)))

(define (GtkIconView-set-pixbuf-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixbuf_column" column)))

(define (GtkIconView-set-reorderable self reorderable)
"  ARGS: 
     reorderable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reorderable" reorderable)))

(define (GtkIconView-set-row-spacing self row-spacing)
"  ARGS: 
     row-spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_spacing" row-spacing)))

(define (GtkIconView-set-selection-mode self mode)
"  ARGS: 
     mode  - exact integer of enum type SelectionMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_selection_mode" mode)))

(define (GtkIconView-set-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_spacing" spacing)))

(define (GtkIconView-set-text-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text_column" column)))

(define (GtkIconView-set-tooltip-cell self tooltip path cell)
"  ARGS: 
     tooltip  - object Tooltip, 
     path  - struct TreePath, 
     cell  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_cell" tooltip path cell)))

(define (GtkIconView-set-tooltip-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_column" column)))

(define (GtkIconView-set-tooltip-item self tooltip path)
"  ARGS: 
     tooltip  - object Tooltip, 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_item" tooltip path)))

(define (GtkIconView-unselect-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_all")))

(define (GtkIconView-unselect-path self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_path" path)))

(define (GtkIconView-unset-model-drag-dest self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_model_drag_dest")))

(define (GtkIconView-unset-model-drag-source self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_model_drag_source")))

(define <GtkIconViewAccessible>
  (gi-lookup-type "Gtk-IconViewAccessible"))

(define ICON_VIEW_DROP_POSITION_NO_DROP
  (gi-enum-value "Gtk-IconViewDropPosition" "no_drop"))

(define ICON_VIEW_DROP_POSITION_DROP_INTO
  (gi-enum-value "Gtk-IconViewDropPosition" "drop_into"))

(define ICON_VIEW_DROP_POSITION_DROP_LEFT
  (gi-enum-value "Gtk-IconViewDropPosition" "drop_left"))

(define ICON_VIEW_DROP_POSITION_DROP_RIGHT
  (gi-enum-value "Gtk-IconViewDropPosition" "drop_right"))

(define ICON_VIEW_DROP_POSITION_DROP_ABOVE
  (gi-enum-value "Gtk-IconViewDropPosition" "drop_above"))

(define ICON_VIEW_DROP_POSITION_DROP_BELOW
  (gi-enum-value "Gtk-IconViewDropPosition" "drop_below"))

;; CALLBACK
(define icon-view-foreach-func
  (gi-lookup-callback-info "Gtk-IconViewForeachFunc"))
;; ARGS: 
;;   icon-view  - object IconView, 
;;   path  - struct TreePath, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define <GtkImage>
  (gi-lookup-type "Gtk-Image"))

(define (GtkImage-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new"))

(define (GtkImage-new-from-animation animation)
"  ARGS: 
     animation  - object PixbufAnimation
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_animation" animation))

(define (GtkImage-new-from-file filename)
"  ARGS: 
     filename  - locale string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_file" filename))

(define (GtkImage-new-from-gicon icon size)
"  ARGS: 
     icon  - Unhandled argument type tag 16, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_gicon" icon size))

(define (GtkImage-new-from-icon-name icon-name size)
"  ARGS: 
     icon-name  - #f for NULL or string, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_icon_name" icon-name size))

(define (GtkImage-new-from-icon-set icon-set size)
"  ARGS: 
     icon-set  - struct IconSet, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_icon_set" icon-set size))

(define (GtkImage-new-from-pixbuf pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_pixbuf" pixbuf))

(define (GtkImage-new-from-resource resource-path)
"  ARGS: 
     resource-path  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_resource" resource-path))

(define (GtkImage-new-from-stock stock-id size)
"  ARGS: 
     stock-id  - string, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_stock" stock-id size))

(define (GtkImage-new-from-surface surface)
"  ARGS: 
     surface  - struct Surface
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Image-new_from_surface" surface))

(define (GtkImage-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkImage-get-animation self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_animation")))

(define (GtkImage-get-gicon self)
"  ARGS: 
     gicon  - Unhandled argument type tag 16[OUT], 
     size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_gicon")))

(define (GtkImage-get-icon-name self)
"  ARGS: 
     icon-name  - string[OUT], 
     size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkImage-get-icon-set self)
"  ARGS: 
     icon-set  - struct IconSet[OUT], 
     size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_set")))

(define (GtkImage-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkImage-get-pixel-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_pixel_size")))

(define (GtkImage-get-stock self)
"  ARGS: 
     stock-id  - string[OUT], 
     size  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_stock")))

(define (GtkImage-get-storage-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_storage_type")))

(define (GtkImage-set-from-animation self animation)
"  ARGS: 
     animation  - object PixbufAnimation
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_animation" animation)))

(define (GtkImage-set-from-file self filename)
"  ARGS: 
     filename  - #f for NULL or locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_file" filename)))

(define (GtkImage-set-from-gicon self icon size)
"  ARGS: 
     icon  - Unhandled argument type tag 16, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_gicon" icon size)))

(define (GtkImage-set-from-icon-name self icon-name size)
"  ARGS: 
     icon-name  - #f for NULL or string, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_icon_name" icon-name size)))

(define (GtkImage-set-from-icon-set self icon-set size)
"  ARGS: 
     icon-set  - struct IconSet, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_icon_set" icon-set size)))

(define (GtkImage-set-from-pixbuf self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_pixbuf" pixbuf)))

(define (GtkImage-set-from-resource self resource-path)
"  ARGS: 
     resource-path  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_resource" resource-path)))

(define (GtkImage-set-from-stock self stock-id size)
"  ARGS: 
     stock-id  - string, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_stock" stock-id size)))

(define (GtkImage-set-from-surface self surface)
"  ARGS: 
     surface  - struct Surface
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_surface" surface)))

(define (GtkImage-set-pixel-size self pixel-size)
"  ARGS: 
     pixel-size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixel_size" pixel-size)))

(define <GtkImageAccessible>
  (gi-lookup-type "Gtk-ImageAccessible"))

(define <GtkImageCellAccessible>
  (gi-lookup-type "Gtk-ImageCellAccessible"))

(define <GtkImageMenuItem>
  (gi-lookup-type "Gtk-ImageMenuItem"))

(define (GtkImageMenuItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ImageMenuItem-new"))

(define (GtkImageMenuItem-new-from-stock stock-id accel-group)
"  ARGS: 
     stock-id  - string, 
     accel-group  - object AccelGroup
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ImageMenuItem-new_from_stock" stock-id accel-group))

(define (GtkImageMenuItem-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ImageMenuItem-new_with_label" label))

(define (GtkImageMenuItem-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ImageMenuItem-new_with_mnemonic" label))

(define (GtkImageMenuItem-get-always-show-image? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_always_show_image")))

(define (GtkImageMenuItem-get-image self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_image")))

(define (GtkImageMenuItem-get-use-stock? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_stock")))

(define (GtkImageMenuItem-set-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_group" accel-group)))

(define (GtkImageMenuItem-set-always-show-image self always-show)
"  ARGS: 
     always-show  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_always_show_image" always-show)))

(define (GtkImageMenuItem-set-image self image)
"  ARGS: 
     image  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_image" image)))

(define (GtkImageMenuItem-set-use-stock self use-stock)
"  ARGS: 
     use-stock  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_stock" use-stock)))

(define IMAGE_TYPE_EMPTY
  (gi-enum-value "Gtk-ImageType" "empty"))

(define IMAGE_TYPE_PIXBUF
  (gi-enum-value "Gtk-ImageType" "pixbuf"))

(define IMAGE_TYPE_STOCK
  (gi-enum-value "Gtk-ImageType" "stock"))

(define IMAGE_TYPE_ICON_SET
  (gi-enum-value "Gtk-ImageType" "icon_set"))

(define IMAGE_TYPE_ANIMATION
  (gi-enum-value "Gtk-ImageType" "animation"))

(define IMAGE_TYPE_ICON_NAME
  (gi-enum-value "Gtk-ImageType" "icon_name"))

(define IMAGE_TYPE_GICON
  (gi-enum-value "Gtk-ImageType" "gicon"))

(define IMAGE_TYPE_SURFACE
  (gi-enum-value "Gtk-ImageType" "surface"))

(define <GtkInfoBar>
  (gi-lookup-type "Gtk-InfoBar"))

(define (GtkInfoBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-InfoBar-new"))

(define (GtkInfoBar-add-action-widget self child response-id)
"  ARGS: 
     child  - object Widget, 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_action_widget" child response-id)))

(define (GtkInfoBar-add-button self button-text response-id)
"  ARGS: 
     button-text  - string, 
     response-id  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "add_button" button-text response-id)))

(define (GtkInfoBar-get-action-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action_area")))

(define (GtkInfoBar-get-content-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_content_area")))

(define (GtkInfoBar-get-message-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_message_type")))

(define (GtkInfoBar-get-revealed? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_revealed")))

(define (GtkInfoBar-get-show-close-button? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_close_button")))

(define (GtkInfoBar-response self response-id)
"  ARGS: 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "response" response-id)))

(define (GtkInfoBar-set-default-response self response-id)
"  ARGS: 
     response-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_response" response-id)))

(define (GtkInfoBar-set-message-type self message-type)
"  ARGS: 
     message-type  - exact integer of enum type MessageType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_message_type" message-type)))

(define (GtkInfoBar-set-response-sensitive self response-id setting)
"  ARGS: 
     response-id  - exact integer of size gint32, 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_response_sensitive" response-id setting)))

(define (GtkInfoBar-set-revealed self revealed)
"  ARGS: 
     revealed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_revealed" revealed)))

(define (GtkInfoBar-set-show-close-button self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_close_button" setting)))

(define NONE
  (gi-flag-value "Gtk-InputHints" "none"))

(define SPELLCHECK
  (gi-flag-value "Gtk-InputHints" "spellcheck"))

(define NO_SPELLCHECK
  (gi-flag-value "Gtk-InputHints" "no_spellcheck"))

(define WORD_COMPLETION
  (gi-flag-value "Gtk-InputHints" "word_completion"))

(define LOWERCASE
  (gi-flag-value "Gtk-InputHints" "lowercase"))

(define UPPERCASE_CHARS
  (gi-flag-value "Gtk-InputHints" "uppercase_chars"))

(define UPPERCASE_WORDS
  (gi-flag-value "Gtk-InputHints" "uppercase_words"))

(define UPPERCASE_SENTENCES
  (gi-flag-value "Gtk-InputHints" "uppercase_sentences"))

(define INHIBIT_OSK
  (gi-flag-value "Gtk-InputHints" "inhibit_osk"))

(define VERTICAL_WRITING
  (gi-flag-value "Gtk-InputHints" "vertical_writing"))

(define EMOJI
  (gi-flag-value "Gtk-InputHints" "emoji"))

(define NO_EMOJI
  (gi-flag-value "Gtk-InputHints" "no_emoji"))

(define INPUT_PURPOSE_FREE_FORM
  (gi-enum-value "Gtk-InputPurpose" "free_form"))

(define INPUT_PURPOSE_ALPHA
  (gi-enum-value "Gtk-InputPurpose" "alpha"))

(define INPUT_PURPOSE_DIGITS
  (gi-enum-value "Gtk-InputPurpose" "digits"))

(define INPUT_PURPOSE_NUMBER
  (gi-enum-value "Gtk-InputPurpose" "number"))

(define INPUT_PURPOSE_PHONE
  (gi-enum-value "Gtk-InputPurpose" "phone"))

(define INPUT_PURPOSE_URL
  (gi-enum-value "Gtk-InputPurpose" "url"))

(define INPUT_PURPOSE_EMAIL
  (gi-enum-value "Gtk-InputPurpose" "email"))

(define INPUT_PURPOSE_NAME
  (gi-enum-value "Gtk-InputPurpose" "name"))

(define INPUT_PURPOSE_PASSWORD
  (gi-enum-value "Gtk-InputPurpose" "password"))

(define INPUT_PURPOSE_PIN
  (gi-enum-value "Gtk-InputPurpose" "pin"))

(define <GtkInvisible>
  (gi-lookup-type "Gtk-Invisible"))

(define (GtkInvisible-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Invisible-new"))

(define (GtkInvisible-new-for-screen screen)
"  ARGS: 
     screen  - object Screen
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Invisible-new_for_screen" screen))

(define (GtkInvisible-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkInvisible-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define NONE
  (gi-flag-value "Gtk-JunctionSides" "none"))

(define CORNER_TOPLEFT
  (gi-flag-value "Gtk-JunctionSides" "corner_topleft"))

(define CORNER_TOPRIGHT
  (gi-flag-value "Gtk-JunctionSides" "corner_topright"))

(define CORNER_BOTTOMLEFT
  (gi-flag-value "Gtk-JunctionSides" "corner_bottomleft"))

(define CORNER_BOTTOMRIGHT
  (gi-flag-value "Gtk-JunctionSides" "corner_bottomright"))

(define TOP
  (gi-flag-value "Gtk-JunctionSides" "top"))

(define BOTTOM
  (gi-flag-value "Gtk-JunctionSides" "bottom"))

(define LEFT
  (gi-flag-value "Gtk-JunctionSides" "left"))

(define RIGHT
  (gi-flag-value "Gtk-JunctionSides" "right"))

(define JUSTIFICATION_LEFT
  (gi-enum-value "Gtk-Justification" "left"))

(define JUSTIFICATION_RIGHT
  (gi-enum-value "Gtk-Justification" "right"))

(define JUSTIFICATION_CENTER
  (gi-enum-value "Gtk-Justification" "center"))

(define JUSTIFICATION_FILL
  (gi-enum-value "Gtk-Justification" "fill"))

;; CALLBACK
(define key-snoop-func
  (gi-lookup-callback-info "Gtk-KeySnoopFunc"))
;; ARGS: 
;;   grab-widget  - object Widget, 
;;   event  - struct EventKey, 
;;   func-data  - #f for NULL or pointer
;; RETURN: gint32
(define GTK_LEVEL_BAR_OFFSET_FULL
  (gi-constant-value "Gtk-LEVEL_BAR_OFFSET_FULL"))

(define GTK_LEVEL_BAR_OFFSET_HIGH
  (gi-constant-value "Gtk-LEVEL_BAR_OFFSET_HIGH"))

(define GTK_LEVEL_BAR_OFFSET_LOW
  (gi-constant-value "Gtk-LEVEL_BAR_OFFSET_LOW"))

(define <GtkLabel>
  (gi-lookup-type "Gtk-Label"))

(define (GtkLabel-new str)
"  ARGS: 
     str  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Label-new" str))

(define (GtkLabel-new-with-mnemonic str)
"  ARGS: 
     str  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Label-new_with_mnemonic" str))

(define (GtkLabel-get-angle self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_angle")))

(define (GtkLabel-get-attributes self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_attributes")))

(define (GtkLabel-get-current-uri self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_current_uri")))

(define (GtkLabel-get-ellipsize self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_ellipsize")))

(define (GtkLabel-get-justify self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_justify")))

(define (GtkLabel-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkLabel-get-layout self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_layout")))

(define (GtkLabel-get-layout-offsets self)
"  ARGS: 
     x  - exact integer of size gint32[OUT], 
     y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_layout_offsets")))

(define (GtkLabel-get-line-wrap? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_line_wrap")))

(define (GtkLabel-get-line-wrap-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_line_wrap_mode")))

(define (GtkLabel-get-lines self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_lines")))

(define (GtkLabel-get-max-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_width_chars")))

(define (GtkLabel-get-mnemonic-keyval self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_mnemonic_keyval")))

(define (GtkLabel-get-mnemonic-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_mnemonic_widget")))

(define (GtkLabel-get-selectable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_selectable")))

(define (GtkLabel-get-selection-bounds? self)
"  ARGS: 
     start  - exact integer of size gint32[OUT], 
     end  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_bounds")))

(define (GtkLabel-get-single-line-mode? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_single_line_mode")))

(define (GtkLabel-get-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text")))

(define (GtkLabel-get-track-visited-links? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_track_visited_links")))

(define (GtkLabel-get-use-markup? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_markup")))

(define (GtkLabel-get-use-underline? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_underline")))

(define (GtkLabel-get-width-chars self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_width_chars")))

(define (GtkLabel-get-xalign self)
"  ARGS: 
   RETURN: gfloat
"
  (gi-method-send self 
     (gi-method-prepare "get_xalign")))

(define (GtkLabel-get-yalign self)
"  ARGS: 
   RETURN: gfloat
"
  (gi-method-send self 
     (gi-method-prepare "get_yalign")))

(define (GtkLabel-select-region self start-offset end-offset)
"  ARGS: 
     start-offset  - exact integer of size gint32, 
     end-offset  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_region" start-offset end-offset)))

(define (GtkLabel-set-angle self angle)
"  ARGS: 
     angle  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_angle" angle)))

(define (GtkLabel-set-attributes self attrs)
"  ARGS: 
     attrs  - struct AttrList
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_attributes" attrs)))

(define (GtkLabel-set-ellipsize self mode)
"  ARGS: 
     mode  - exact integer of enum type EllipsizeMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_ellipsize" mode)))

(define (GtkLabel-set-justify self jtype)
"  ARGS: 
     jtype  - exact integer of enum type Justification
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_justify" jtype)))

(define (GtkLabel-set-label self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" str)))

(define (GtkLabel-set-line-wrap self wrap)
"  ARGS: 
     wrap  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_line_wrap" wrap)))

(define (GtkLabel-set-line-wrap-mode self wrap-mode)
"  ARGS: 
     wrap-mode  - exact integer of enum type WrapMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_line_wrap_mode" wrap-mode)))

(define (GtkLabel-set-lines self lines)
"  ARGS: 
     lines  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_lines" lines)))

(define (GtkLabel-set-markup self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_markup" str)))

(define (GtkLabel-set-markup-with-mnemonic self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_markup_with_mnemonic" str)))

(define (GtkLabel-set-max-width-chars self n-chars)
"  ARGS: 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_width_chars" n-chars)))

(define (GtkLabel-set-mnemonic-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mnemonic_widget" widget)))

(define (GtkLabel-set-pattern self pattern)
"  ARGS: 
     pattern  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pattern" pattern)))

(define (GtkLabel-set-selectable self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_selectable" setting)))

(define (GtkLabel-set-single-line-mode self single-line-mode)
"  ARGS: 
     single-line-mode  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_single_line_mode" single-line-mode)))

(define (GtkLabel-set-text self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" str)))

(define (GtkLabel-set-text-with-mnemonic self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text_with_mnemonic" str)))

(define (GtkLabel-set-track-visited-links self track-links)
"  ARGS: 
     track-links  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_track_visited_links" track-links)))

(define (GtkLabel-set-use-markup self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_markup" setting)))

(define (GtkLabel-set-use-underline self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_underline" setting)))

(define (GtkLabel-set-width-chars self n-chars)
"  ARGS: 
     n-chars  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_width_chars" n-chars)))

(define (GtkLabel-set-xalign self xalign)
"  ARGS: 
     xalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_xalign" xalign)))

(define (GtkLabel-set-yalign self yalign)
"  ARGS: 
     yalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_yalign" yalign)))

(define <GtkLabelAccessible>
  (gi-lookup-type "Gtk-LabelAccessible"))

(define <GtkLayout>
  (gi-lookup-type "Gtk-Layout"))

(define (GtkLayout-new hadjustment vadjustment)
"  ARGS: 
     hadjustment  - object Adjustment, 
     vadjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Layout-new" hadjustment vadjustment))

(define (GtkLayout-get-bin-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_bin_window")))

(define (GtkLayout-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkLayout-get-size self)
"  ARGS: 
     width  - exact integer of size guint32[OUT], 
     height  - exact integer of size guint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkLayout-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkLayout-move self child-widget x y)
"  ARGS: 
     child-widget  - object Widget, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move" child-widget x y)))

(define (GtkLayout-put self child-widget x y)
"  ARGS: 
     child-widget  - object Widget, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "put" child-widget x y)))

(define (GtkLayout-set-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hadjustment" adjustment)))

(define (GtkLayout-set-size self width height)
"  ARGS: 
     width  - exact integer of size guint32, 
     height  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_size" width height)))

(define (GtkLayout-set-vadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vadjustment" adjustment)))

(define <GtkLevelBar>
  (gi-lookup-type "Gtk-LevelBar"))

(define (GtkLevelBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-LevelBar-new"))

(define (GtkLevelBar-new-for-interval min-value max-value)
"  ARGS: 
     min-value  - real number of size gdouble, 
     max-value  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-LevelBar-new_for_interval" min-value max-value))

(define (GtkLevelBar-add-offset-value self name value)
"  ARGS: 
     name  - string, 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_offset_value" name value)))

(define (GtkLevelBar-get-inverted? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inverted")))

(define (GtkLevelBar-get-max-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_max_value")))

(define (GtkLevelBar-get-min-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_min_value")))

(define (GtkLevelBar-get-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_mode")))

(define (GtkLevelBar-get-offset-value? self name)
"  ARGS: 
     name  - #f for NULL or string, 
     value  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_offset_value" name)))

(define (GtkLevelBar-get-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_value")))

(define (GtkLevelBar-remove-offset-value self name)
"  ARGS: 
     name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_offset_value" name)))

(define (GtkLevelBar-set-inverted self inverted)
"  ARGS: 
     inverted  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inverted" inverted)))

(define (GtkLevelBar-set-max-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_value" value)))

(define (GtkLevelBar-set-min-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_value" value)))

(define (GtkLevelBar-set-mode self mode)
"  ARGS: 
     mode  - exact integer of enum type LevelBarMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mode" mode)))

(define (GtkLevelBar-set-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" value)))

(define <GtkLevelBarAccessible>
  (gi-lookup-type "Gtk-LevelBarAccessible"))

(define LEVEL_BAR_MODE_CONTINUOUS
  (gi-enum-value "Gtk-LevelBarMode" "continuous"))

(define LEVEL_BAR_MODE_DISCRETE
  (gi-enum-value "Gtk-LevelBarMode" "discrete"))

(define LICENSE_UNKNOWN
  (gi-enum-value "Gtk-License" "unknown"))

(define LICENSE_CUSTOM
  (gi-enum-value "Gtk-License" "custom"))

(define LICENSE_GPL_2_0
  (gi-enum-value "Gtk-License" "gpl_2_0"))

(define LICENSE_GPL_3_0
  (gi-enum-value "Gtk-License" "gpl_3_0"))

(define LICENSE_LGPL_2_1
  (gi-enum-value "Gtk-License" "lgpl_2_1"))

(define LICENSE_LGPL_3_0
  (gi-enum-value "Gtk-License" "lgpl_3_0"))

(define LICENSE_BSD
  (gi-enum-value "Gtk-License" "bsd"))

(define LICENSE_MIT_X11
  (gi-enum-value "Gtk-License" "mit_x11"))

(define LICENSE_ARTISTIC
  (gi-enum-value "Gtk-License" "artistic"))

(define LICENSE_GPL_2_0_ONLY
  (gi-enum-value "Gtk-License" "gpl_2_0_only"))

(define LICENSE_GPL_3_0_ONLY
  (gi-enum-value "Gtk-License" "gpl_3_0_only"))

(define LICENSE_LGPL_2_1_ONLY
  (gi-enum-value "Gtk-License" "lgpl_2_1_only"))

(define LICENSE_LGPL_3_0_ONLY
  (gi-enum-value "Gtk-License" "lgpl_3_0_only"))

(define LICENSE_AGPL_3_0
  (gi-enum-value "Gtk-License" "agpl_3_0"))

(define LICENSE_AGPL_3_0_ONLY
  (gi-enum-value "Gtk-License" "agpl_3_0_only"))

(define <GtkLinkButton>
  (gi-lookup-type "Gtk-LinkButton"))

(define (GtkLinkButton-new uri)
"  ARGS: 
     uri  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-LinkButton-new" uri))

(define (GtkLinkButton-new-with-label uri label)
"  ARGS: 
     uri  - string, 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-LinkButton-new_with_label" uri label))

(define (GtkLinkButton-get-uri self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_uri")))

(define (GtkLinkButton-get-visited? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visited")))

(define (GtkLinkButton-set-uri self uri)
"  ARGS: 
     uri  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_uri" uri)))

(define (GtkLinkButton-set-visited self visited)
"  ARGS: 
     visited  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visited" visited)))

(define <GtkLinkButtonAccessible>
  (gi-lookup-type "Gtk-LinkButtonAccessible"))

(define <GtkListBox>
  (gi-lookup-type "Gtk-ListBox"))

(define (GtkListBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ListBox-new"))

(define (GtkListBox-bind-model self model create-widget-func user-data user-data-free-func)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16, 
     create-widget-func  - procedure of type ListBoxCreateWidgetFunc, 
     user-data  - #f for NULL or pointer, 
     user-data-free-func  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "bind_model" model create-widget-func user-data user-data-free-func)))

(define (GtkListBox-drag-highlight-row self row)
"  ARGS: 
     row  - object ListBoxRow
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_highlight_row" row)))

(define (GtkListBox-drag-unhighlight-row self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_unhighlight_row")))

(define (GtkListBox-get-activate-on-single-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activate_on_single_click")))

(define (GtkListBox-get-adjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_adjustment")))

(define (GtkListBox-get-row-at-index self index-)
"  ARGS: 
     index-  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_row_at_index" index-)))

(define (GtkListBox-get-row-at-y self y)
"  ARGS: 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_row_at_y" y)))

(define (GtkListBox-get-selected-row self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_row")))

(define (GtkListBox-get-selected-rows self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_rows")))

(define (GtkListBox-get-selection-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_mode")))

(define (GtkListBox-insert self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" child position)))

(define (GtkListBox-invalidate-filter self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate_filter")))

(define (GtkListBox-invalidate-headers self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate_headers")))

(define (GtkListBox-invalidate-sort self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate_sort")))

(define (GtkListBox-prepend self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend" child)))

(define (GtkListBox-select-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_all")))

(define (GtkListBox-select-row self row)
"  ARGS: 
     row  - object ListBoxRow
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_row" row)))

(define (GtkListBox-selected-foreach self func data)
"  ARGS: 
     func  - procedure of type ListBoxForeachFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "selected_foreach" func data)))

(define (GtkListBox-set-activate-on-single-click self single)
"  ARGS: 
     single  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activate_on_single_click" single)))

(define (GtkListBox-set-adjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_adjustment" adjustment)))

(define (GtkListBox-set-filter-func self filter-func user-data destroy)
"  ARGS: 
     filter-func  - procedure of type ListBoxFilterFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_filter_func" filter-func user-data destroy)))

(define (GtkListBox-set-header-func self update-header user-data destroy)
"  ARGS: 
     update-header  - procedure of type ListBoxUpdateHeaderFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_header_func" update-header user-data destroy)))

(define (GtkListBox-set-placeholder self placeholder)
"  ARGS: 
     placeholder  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_placeholder" placeholder)))

(define (GtkListBox-set-selection-mode self mode)
"  ARGS: 
     mode  - exact integer of enum type SelectionMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_selection_mode" mode)))

(define (GtkListBox-set-sort-func self sort-func user-data destroy)
"  ARGS: 
     sort-func  - procedure of type ListBoxSortFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sort_func" sort-func user-data destroy)))

(define (GtkListBox-unselect-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_all")))

(define (GtkListBox-unselect-row self row)
"  ARGS: 
     row  - object ListBoxRow
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_row" row)))

(define <GtkListBoxAccessible>
  (gi-lookup-type "Gtk-ListBoxAccessible"))

;; CALLBACK
(define list-box-create-widget-func
  (gi-lookup-callback-info "Gtk-ListBoxCreateWidgetFunc"))
;; ARGS: 
;;   item  - object Object, 
;;   user-data  - #f for NULL or pointer
;; RETURN: interface*
;; CALLBACK
(define list-box-filter-func
  (gi-lookup-callback-info "Gtk-ListBoxFilterFunc"))
;; ARGS: 
;;   row  - object ListBoxRow, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define list-box-foreach-func
  (gi-lookup-callback-info "Gtk-ListBoxForeachFunc"))
;; ARGS: 
;;   box  - object ListBox, 
;;   row  - object ListBoxRow, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define <GtkListBoxRow>
  (gi-lookup-type "Gtk-ListBoxRow"))

(define (GtkListBoxRow-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ListBoxRow-new"))

(define (GtkListBoxRow-changed self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "changed")))

(define (GtkListBoxRow-get-activatable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activatable")))

(define (GtkListBoxRow-get-header self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_header")))

(define (GtkListBoxRow-get-index self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_index")))

(define (GtkListBoxRow-get-selectable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_selectable")))

(define (GtkListBoxRow-is-selected? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_selected")))

(define (GtkListBoxRow-set-activatable self activatable)
"  ARGS: 
     activatable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activatable" activatable)))

(define (GtkListBoxRow-set-header self header)
"  ARGS: 
     header  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_header" header)))

(define (GtkListBoxRow-set-selectable self selectable)
"  ARGS: 
     selectable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_selectable" selectable)))

(define <GtkListBoxRowAccessible>
  (gi-lookup-type "Gtk-ListBoxRowAccessible"))

;; CALLBACK
(define list-box-sort-func
  (gi-lookup-callback-info "Gtk-ListBoxSortFunc"))
;; ARGS: 
;;   row1  - object ListBoxRow, 
;;   row2  - object ListBoxRow, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gint32
;; CALLBACK
(define list-box-update-header-func
  (gi-lookup-callback-info "Gtk-ListBoxUpdateHeaderFunc"))
;; ARGS: 
;;   row  - object ListBoxRow, 
;;   before  - object ListBoxRow, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define <GtkListStore>
  (gi-lookup-type "Gtk-ListStore"))

(define (GtkListStore-new n-columns types)
"  ARGS: 
     n-columns  - exact integer of size gint32, 
     types  - Unhandled argument type tag 15
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ListStore-new" n-columns types))

(define (GtkListStore-append self out-iter)
"  ARGS: 
   RETURN: void
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "append" out-iter)))

(define (GtkListStore-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkListStore-insert self out-iter position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert" out-iter position)))

(define (GtkListStore-insert-after self out-iter sibling)
"  ARGS: 
     sibling  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_after" out-iter sibling)))

(define (GtkListStore-insert-before self out-iter sibling)
"  ARGS: 
     sibling  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_before" out-iter sibling)))

(define (GtkListStore-insert-with-valuesv self out-iter position columns values n-values)
"  ARGS: 
     position  - exact integer of size gint32, 
     columns  - Unhandled argument type tag 15, 
     values  - Unhandled argument type tag 15, 
     n-values  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_with_valuesv" out-iter position columns values n-values)))

(define (GtkListStore-iter-is-valid? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_is_valid" iter)))

(define (GtkListStore-move-after self iter position)
"  ARGS: 
     iter  - struct TreeIter, 
     position  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_after" iter position)))

(define (GtkListStore-move-before self iter position)
"  ARGS: 
     iter  - struct TreeIter, 
     position  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_before" iter position)))

(define (GtkListStore-prepend self out-iter)
"  ARGS: 
   RETURN: void
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "prepend" out-iter)))

(define (GtkListStore-remove? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "remove" iter)))

(define (GtkListStore-reorder self new-order)
"  ARGS: 
     new-order  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reorder" new-order)))

(define (GtkListStore-set-column-types self n-columns types)
"  ARGS: 
     n-columns  - exact integer of size gint32, 
     types  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_types" n-columns types)))

(define (GtkListStore-set-value self iter column value)
"  ARGS: 
     iter  - struct TreeIter, 
     column  - exact integer of size gint32, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" iter column value)))

(define (GtkListStore-set self iter columns values n-values)
"  ARGS: 
     iter  - struct TreeIter, 
     columns  - Unhandled argument type tag 15, 
     values  - Unhandled argument type tag 15, 
     n-values  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" iter columns values n-values)))

(define (GtkListStore-swap self a b)
"  ARGS: 
     a  - struct TreeIter, 
     b  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "swap" a b)))

(define <GtkLockButton>
  (gi-lookup-type "Gtk-LockButton"))

(define (GtkLockButton-new permission)
"  ARGS: 
     permission  - object Permission
   RETURN: interface*
"
  (gi-function-invoke "Gtk-LockButton-new" permission))

(define (GtkLockButton-get-permission self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_permission")))

(define (GtkLockButton-set-permission self permission)
"  ARGS: 
     permission  - object Permission
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_permission" permission)))

(define <GtkLockButtonAccessible>
  (gi-lookup-type "Gtk-LockButtonAccessible"))

(define GTK_MAJOR_VERSION
  (gi-constant-value "Gtk-MAJOR_VERSION"))

(define GTK_MAX_COMPOSE_LEN
  (gi-constant-value "Gtk-MAX_COMPOSE_LEN"))

(define GTK_MICRO_VERSION
  (gi-constant-value "Gtk-MICRO_VERSION"))

(define GTK_MINOR_VERSION
  (gi-constant-value "Gtk-MINOR_VERSION"))

(define <GtkMenu>
  (gi-lookup-type "Gtk-Menu"))

(define (GtkMenu-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Menu-new"))

(define (GtkMenu-new-from-model model)
"  ARGS: 
     model  - object MenuModel
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Menu-new_from_model" model))

(define (GtkMenu-get-for-attach-widget widget)
"  ARGS: 
     widget  - object Widget
   RETURN: glist*
"
  (gi-function-invoke "Gtk-Menu-get_for_attach_widget" widget))

(define (GtkMenu-attach self child left-attach right-attach top-attach bottom-attach)
"  ARGS: 
     child  - object Widget, 
     left-attach  - exact integer of size guint32, 
     right-attach  - exact integer of size guint32, 
     top-attach  - exact integer of size guint32, 
     bottom-attach  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach" child left-attach right-attach top-attach bottom-attach)))

(define (GtkMenu-attach-to-widget self attach-widget detacher)
"  ARGS: 
     attach-widget  - object Widget, 
     detacher  - procedure of type MenuDetachFunc
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach_to_widget" attach-widget detacher)))

(define (GtkMenu-detach self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "detach")))

(define (GtkMenu-get-accel-group self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_group")))

(define (GtkMenu-get-accel-path self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_path")))

(define (GtkMenu-get-active self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkMenu-get-attach-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_attach_widget")))

(define (GtkMenu-get-monitor self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_monitor")))

(define (GtkMenu-get-reserve-toggle-size? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reserve_toggle_size")))

(define (GtkMenu-get-tearoff-state? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_tearoff_state")))

(define (GtkMenu-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkMenu-place-on-monitor self monitor)
"  ARGS: 
     monitor  - object Monitor
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "place_on_monitor" monitor)))

(define (GtkMenu-popdown self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popdown")))

(define (GtkMenu-popup self parent-menu-shell parent-menu-item func data button activate-time)
"  ARGS: 
     parent-menu-shell  - object Widget, 
     parent-menu-item  - object Widget, 
     func  - procedure of type MenuPositionFunc, 
     data  - #f for NULL or pointer, 
     button  - exact integer of size guint32, 
     activate-time  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup" parent-menu-shell parent-menu-item func data button activate-time)))

(define (GtkMenu-popup-at-pointer self trigger-event)
"  ARGS: 
     trigger-event  - union Event
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_at_pointer" trigger-event)))

(define (GtkMenu-popup-at-rect self rect-window rect rect-anchor menu-anchor trigger-event)
"  ARGS: 
     rect-window  - object Window, 
     rect  - struct Rectangle, 
     rect-anchor  - exact integer of enum type Gravity, 
     menu-anchor  - exact integer of enum type Gravity, 
     trigger-event  - union Event
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_at_rect" rect-window rect rect-anchor menu-anchor trigger-event)))

(define (GtkMenu-popup-at-widget self widget widget-anchor menu-anchor trigger-event)
"  ARGS: 
     widget  - object Widget, 
     widget-anchor  - exact integer of enum type Gravity, 
     menu-anchor  - exact integer of enum type Gravity, 
     trigger-event  - union Event
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_at_widget" widget widget-anchor menu-anchor trigger-event)))

(define (GtkMenu-popup-for-device self device parent-menu-shell parent-menu-item func data destroy button activate-time)
"  ARGS: 
     device  - object Device, 
     parent-menu-shell  - object Widget, 
     parent-menu-item  - object Widget, 
     func  - procedure of type MenuPositionFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify, 
     button  - exact integer of size guint32, 
     activate-time  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_for_device" device parent-menu-shell parent-menu-item func data destroy button activate-time)))

(define (GtkMenu-reorder-child self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reorder_child" child position)))

(define (GtkMenu-reposition self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reposition")))

(define (GtkMenu-set-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_group" accel-group)))

(define (GtkMenu-set-accel-path self accel-path)
"  ARGS: 
     accel-path  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_path" accel-path)))

(define (GtkMenu-set-active self index)
"  ARGS: 
     index  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" index)))

(define (GtkMenu-set-monitor self monitor-num)
"  ARGS: 
     monitor-num  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_monitor" monitor-num)))

(define (GtkMenu-set-reserve-toggle-size self reserve-toggle-size)
"  ARGS: 
     reserve-toggle-size  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reserve_toggle_size" reserve-toggle-size)))

(define (GtkMenu-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define (GtkMenu-set-tearoff-state self torn-off)
"  ARGS: 
     torn-off  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tearoff_state" torn-off)))

(define (GtkMenu-set-title self title)
"  ARGS: 
     title  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define <GtkMenuAccessible>
  (gi-lookup-type "Gtk-MenuAccessible"))

(define <GtkMenuBar>
  (gi-lookup-type "Gtk-MenuBar"))

(define (GtkMenuBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuBar-new"))

(define (GtkMenuBar-new-from-model model)
"  ARGS: 
     model  - object MenuModel
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuBar-new_from_model" model))

(define (GtkMenuBar-get-child-pack-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_child_pack_direction")))

(define (GtkMenuBar-get-pack-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_pack_direction")))

(define (GtkMenuBar-set-child-pack-direction self child-pack-dir)
"  ARGS: 
     child-pack-dir  - exact integer of enum type PackDirection
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_child_pack_direction" child-pack-dir)))

(define (GtkMenuBar-set-pack-direction self pack-dir)
"  ARGS: 
     pack-dir  - exact integer of enum type PackDirection
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pack_direction" pack-dir)))

(define <GtkMenuButton>
  (gi-lookup-type "Gtk-MenuButton"))

(define (GtkMenuButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuButton-new"))

(define (GtkMenuButton-get-align-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_align_widget")))

(define (GtkMenuButton-get-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_direction")))

(define (GtkMenuButton-get-menu-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_menu_model")))

(define (GtkMenuButton-get-popover self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_popover")))

(define (GtkMenuButton-get-popup self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_popup")))

(define (GtkMenuButton-get-use-popover? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_popover")))

(define (GtkMenuButton-set-align-widget self align-widget)
"  ARGS: 
     align-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_align_widget" align-widget)))

(define (GtkMenuButton-set-direction self direction)
"  ARGS: 
     direction  - exact integer of enum type ArrowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_direction" direction)))

(define (GtkMenuButton-set-menu-model self menu-model)
"  ARGS: 
     menu-model  - object MenuModel
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_menu_model" menu-model)))

(define (GtkMenuButton-set-popover self popover)
"  ARGS: 
     popover  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popover" popover)))

(define (GtkMenuButton-set-popup self menu)
"  ARGS: 
     menu  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_popup" menu)))

(define (GtkMenuButton-set-use-popover self use-popover)
"  ARGS: 
     use-popover  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_popover" use-popover)))

(define <GtkMenuButtonAccessible>
  (gi-lookup-type "Gtk-MenuButtonAccessible"))

;; CALLBACK
(define menu-detach-func
  (gi-lookup-callback-info "Gtk-MenuDetachFunc"))
;; ARGS: 
;;   attach-widget  - object Widget, 
;;   menu  - object Menu
;; RETURN: void
(define MENU_DIRECTION_TYPE_PARENT
  (gi-enum-value "Gtk-MenuDirectionType" "parent"))

(define MENU_DIRECTION_TYPE_CHILD
  (gi-enum-value "Gtk-MenuDirectionType" "child"))

(define MENU_DIRECTION_TYPE_NEXT
  (gi-enum-value "Gtk-MenuDirectionType" "next"))

(define MENU_DIRECTION_TYPE_PREV
  (gi-enum-value "Gtk-MenuDirectionType" "prev"))

(define <GtkMenuItem>
  (gi-lookup-type "Gtk-MenuItem"))

(define (GtkMenuItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuItem-new"))

(define (GtkMenuItem-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuItem-new_with_label" label))

(define (GtkMenuItem-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuItem-new_with_mnemonic" label))

(define (GtkMenuItem-activate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "activate")))

(define (GtkMenuItem-deselect self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "deselect")))

(define (GtkMenuItem-get-accel-path self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_path")))

(define (GtkMenuItem-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkMenuItem-get-reserve-indicator? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reserve_indicator")))

(define (GtkMenuItem-get-right-justified? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_right_justified")))

(define (GtkMenuItem-get-submenu self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_submenu")))

(define (GtkMenuItem-get-use-underline? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_underline")))

(define (GtkMenuItem-select self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select")))

(define (GtkMenuItem-set-accel-path self accel-path)
"  ARGS: 
     accel-path  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_path" accel-path)))

(define (GtkMenuItem-set-label self label)
"  ARGS: 
     label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkMenuItem-set-reserve-indicator self reserve)
"  ARGS: 
     reserve  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reserve_indicator" reserve)))

(define (GtkMenuItem-set-right-justified self right-justified)
"  ARGS: 
     right-justified  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_right_justified" right-justified)))

(define (GtkMenuItem-set-submenu self submenu)
"  ARGS: 
     submenu  - object Menu
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_submenu" submenu)))

(define (GtkMenuItem-set-use-underline self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_underline" setting)))

(define (GtkMenuItem-toggle-size-allocate self allocation)
"  ARGS: 
     allocation  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toggle_size_allocate" allocation)))

(define (GtkMenuItem-toggle-size-request self requisition)
"  ARGS: 
     requisition  - exact integer of size gint32[INOUT] 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toggle_size_request" requisition)))

(define <GtkMenuItemAccessible>
  (gi-lookup-type "Gtk-MenuItemAccessible"))

;; CALLBACK
(define menu-position-func
  (gi-lookup-callback-info "Gtk-MenuPositionFunc"))
;; ARGS: 
;;   menu  - object Menu, 
;;   x  - exact integer of size gint32[INOUT] , 
;;   y  - exact integer of size gint32[INOUT] , 
;;   push-in  - boolean[OUT], 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define <GtkMenuShell>
  (gi-lookup-type "Gtk-MenuShell"))

(define (GtkMenuShell-activate-item self menu-item force-deactivate)
"  ARGS: 
     menu-item  - object Widget, 
     force-deactivate  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "activate_item" menu-item force-deactivate)))

(define (GtkMenuShell-append self child)
"  ARGS: 
     child  - object MenuItem
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append" child)))

(define (GtkMenuShell-bind-model self model action-namespace with-separators)
"  ARGS: 
     model  - object MenuModel, 
     action-namespace  - #f for NULL or string, 
     with-separators  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "bind_model" model action-namespace with-separators)))

(define (GtkMenuShell-cancel self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cancel")))

(define (GtkMenuShell-deactivate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "deactivate")))

(define (GtkMenuShell-deselect self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "deselect")))

(define (GtkMenuShell-get-parent-shell self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent_shell")))

(define (GtkMenuShell-get-selected-item self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_item")))

(define (GtkMenuShell-get-take-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_take_focus")))

(define (GtkMenuShell-insert self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" child position)))

(define (GtkMenuShell-prepend self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend" child)))

(define (GtkMenuShell-select-first self search-sensitive)
"  ARGS: 
     search-sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_first" search-sensitive)))

(define (GtkMenuShell-select-item self menu-item)
"  ARGS: 
     menu-item  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_item" menu-item)))

(define (GtkMenuShell-set-take-focus self take-focus)
"  ARGS: 
     take-focus  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_take_focus" take-focus)))

(define <GtkMenuShellAccessible>
  (gi-lookup-type "Gtk-MenuShellAccessible"))

(define <GtkMenuToolButton>
  (gi-lookup-type "Gtk-MenuToolButton"))

(define (GtkMenuToolButton-new icon-widget label)
"  ARGS: 
     icon-widget  - object Widget, 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuToolButton-new" icon-widget label))

(define (GtkMenuToolButton-new-from-stock stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MenuToolButton-new_from_stock" stock-id))

(define (GtkMenuToolButton-get-menu self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_menu")))

(define (GtkMenuToolButton-set-arrow-tooltip-markup self markup)
"  ARGS: 
     markup  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_arrow_tooltip_markup" markup)))

(define (GtkMenuToolButton-set-arrow-tooltip-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_arrow_tooltip_text" text)))

(define (GtkMenuToolButton-set-menu self menu)
"  ARGS: 
     menu  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_menu" menu)))

(define <GtkMessageDialog>
  (gi-lookup-type "Gtk-MessageDialog"))

(define (GtkMessageDialog-get-image self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_image")))

(define (GtkMessageDialog-get-message-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_message_area")))

(define (GtkMessageDialog-set-image self image)
"  ARGS: 
     image  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_image" image)))

(define (GtkMessageDialog-set-markup self str)
"  ARGS: 
     str  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_markup" str)))

(define MESSAGE_TYPE_INFO
  (gi-enum-value "Gtk-MessageType" "info"))

(define MESSAGE_TYPE_WARNING
  (gi-enum-value "Gtk-MessageType" "warning"))

(define MESSAGE_TYPE_QUESTION
  (gi-enum-value "Gtk-MessageType" "question"))

(define MESSAGE_TYPE_ERROR
  (gi-enum-value "Gtk-MessageType" "error"))

(define MESSAGE_TYPE_OTHER
  (gi-enum-value "Gtk-MessageType" "other"))

(define <GtkMisc>
  (gi-lookup-type "Gtk-Misc"))

(define (GtkMisc-get-alignment self)
"  ARGS: 
     xalign  - real number of size gfloat[OUT], 
     yalign  - real number of size gfloat[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_alignment")))

(define (GtkMisc-get-padding self)
"  ARGS: 
     xpad  - exact integer of size gint32[OUT], 
     ypad  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_padding")))

(define (GtkMisc-set-alignment self xalign yalign)
"  ARGS: 
     xalign  - real number of size gfloat, 
     yalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alignment" xalign yalign)))

(define (GtkMisc-set-padding self xpad ypad)
"  ARGS: 
     xpad  - exact integer of size gint32, 
     ypad  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_padding" xpad ypad)))

(define <GtkModelButton>
  (gi-lookup-type "Gtk-ModelButton"))

(define (GtkModelButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ModelButton-new"))

;; CALLBACK
(define module-display-init-func
  (gi-lookup-callback-info "Gtk-ModuleDisplayInitFunc"))
;; ARGS: 
;;   display  - object Display
;; RETURN: void
;; CALLBACK
(define module-init-func
  (gi-lookup-callback-info "Gtk-ModuleInitFunc"))
;; ARGS: 
;;   argc  - #f for NULL or bytevector containing elements gint32, 
;;   argv  - #f for NULL or Unhandled argument type tag 15
;; RETURN: void
(define <GtkMountOperation>
  (gi-lookup-type "Gtk-MountOperation"))

(define (GtkMountOperation-new parent)
"  ARGS: 
     parent  - object Window
   RETURN: interface*
"
  (gi-function-invoke "Gtk-MountOperation-new" parent))

(define (GtkMountOperation-get-parent self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent")))

(define (GtkMountOperation-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkMountOperation-is-showing? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_showing")))

(define (GtkMountOperation-set-parent self parent)
"  ARGS: 
     parent  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_parent" parent)))

(define (GtkMountOperation-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define MOVEMENT_STEP_LOGICAL_POSITIONS
  (gi-enum-value "Gtk-MovementStep" "logical_positions"))

(define MOVEMENT_STEP_VISUAL_POSITIONS
  (gi-enum-value "Gtk-MovementStep" "visual_positions"))

(define MOVEMENT_STEP_WORDS
  (gi-enum-value "Gtk-MovementStep" "words"))

(define MOVEMENT_STEP_DISPLAY_LINES
  (gi-enum-value "Gtk-MovementStep" "display_lines"))

(define MOVEMENT_STEP_DISPLAY_LINE_ENDS
  (gi-enum-value "Gtk-MovementStep" "display_line_ends"))

(define MOVEMENT_STEP_PARAGRAPHS
  (gi-enum-value "Gtk-MovementStep" "paragraphs"))

(define MOVEMENT_STEP_PARAGRAPH_ENDS
  (gi-enum-value "Gtk-MovementStep" "paragraph_ends"))

(define MOVEMENT_STEP_PAGES
  (gi-enum-value "Gtk-MovementStep" "pages"))

(define MOVEMENT_STEP_BUFFER_ENDS
  (gi-enum-value "Gtk-MovementStep" "buffer_ends"))

(define MOVEMENT_STEP_HORIZONTAL_PAGES
  (gi-enum-value "Gtk-MovementStep" "horizontal_pages"))

(define <GtkNativeDialog>
  (gi-lookup-type "Gtk-NativeDialog"))

(define (GtkNativeDialog-destroy self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "destroy")))

(define (GtkNativeDialog-get-modal? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_modal")))

(define (GtkNativeDialog-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkNativeDialog-get-transient-for self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_transient_for")))

(define (GtkNativeDialog-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkNativeDialog-hide self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "hide")))

(define (GtkNativeDialog-run self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "run")))

(define (GtkNativeDialog-set-modal self modal)
"  ARGS: 
     modal  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_modal" modal)))

(define (GtkNativeDialog-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkNativeDialog-set-transient-for self parent)
"  ARGS: 
     parent  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transient_for" parent)))

(define (GtkNativeDialog-show self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "show")))

(define <GtkNotebook>
  (gi-lookup-type "Gtk-Notebook"))

(define (GtkNotebook-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Notebook-new"))

(define (GtkNotebook-append-page self child tab-label)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_page" child tab-label)))

(define (GtkNotebook-append-page-menu self child tab-label menu-label)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget, 
     menu-label  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_page_menu" child tab-label menu-label)))

(define (GtkNotebook-detach-tab self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "detach_tab" child)))

(define (GtkNotebook-get-action-widget self pack-type)
"  ARGS: 
     pack-type  - exact integer of enum type PackType
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action_widget" pack-type)))

(define (GtkNotebook-get-current-page self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_current_page")))

(define (GtkNotebook-get-group-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_group_name")))

(define (GtkNotebook-get-menu-label self child)
"  ARGS: 
     child  - object Widget
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_menu_label" child)))

(define (GtkNotebook-get-menu-label-text self child)
"  ARGS: 
     child  - object Widget
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_menu_label_text" child)))

(define (GtkNotebook-get-n-pages self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_pages")))

(define (GtkNotebook-get-nth-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_nth_page" page-num)))

(define (GtkNotebook-get-scrollable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_scrollable")))

(define (GtkNotebook-get-show-border? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_border")))

(define (GtkNotebook-get-show-tabs? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_tabs")))

(define (GtkNotebook-get-tab-detachable? self child)
"  ARGS: 
     child  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_detachable" child)))

(define (GtkNotebook-get-tab-hborder self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_hborder")))

(define (GtkNotebook-get-tab-label self child)
"  ARGS: 
     child  - object Widget
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_label" child)))

(define (GtkNotebook-get-tab-label-text self child)
"  ARGS: 
     child  - object Widget
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_label_text" child)))

(define (GtkNotebook-get-tab-pos self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_pos")))

(define (GtkNotebook-get-tab-reorderable? self child)
"  ARGS: 
     child  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_reorderable" child)))

(define (GtkNotebook-get-tab-vborder self)
"  ARGS: 
   RETURN: guint16
"
  (gi-method-send self 
     (gi-method-prepare "get_tab_vborder")))

(define (GtkNotebook-insert-page self child tab-label position)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget, 
     position  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_page" child tab-label position)))

(define (GtkNotebook-insert-page-menu self child tab-label menu-label position)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget, 
     menu-label  - object Widget, 
     position  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_page_menu" child tab-label menu-label position)))

(define (GtkNotebook-next-page self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "next_page")))

(define (GtkNotebook-page-num self child)
"  ARGS: 
     child  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "page_num" child)))

(define (GtkNotebook-popup-disable self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_disable")))

(define (GtkNotebook-popup-enable self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup_enable")))

(define (GtkNotebook-prepend-page self child tab-label)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "prepend_page" child tab-label)))

(define (GtkNotebook-prepend-page-menu self child tab-label menu-label)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget, 
     menu-label  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "prepend_page_menu" child tab-label menu-label)))

(define (GtkNotebook-prev-page self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prev_page")))

(define (GtkNotebook-remove-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_page" page-num)))

(define (GtkNotebook-reorder-child self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reorder_child" child position)))

(define (GtkNotebook-set-action-widget self widget pack-type)
"  ARGS: 
     widget  - object Widget, 
     pack-type  - exact integer of enum type PackType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_action_widget" widget pack-type)))

(define (GtkNotebook-set-current-page self page-num)
"  ARGS: 
     page-num  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_page" page-num)))

(define (GtkNotebook-set-group-name self group-name)
"  ARGS: 
     group-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group_name" group-name)))

(define (GtkNotebook-set-menu-label self child menu-label)
"  ARGS: 
     child  - object Widget, 
     menu-label  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_menu_label" child menu-label)))

(define (GtkNotebook-set-menu-label-text self child menu-text)
"  ARGS: 
     child  - object Widget, 
     menu-text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_menu_label_text" child menu-text)))

(define (GtkNotebook-set-scrollable self scrollable)
"  ARGS: 
     scrollable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_scrollable" scrollable)))

(define (GtkNotebook-set-show-border self show-border)
"  ARGS: 
     show-border  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_border" show-border)))

(define (GtkNotebook-set-show-tabs self show-tabs)
"  ARGS: 
     show-tabs  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_tabs" show-tabs)))

(define (GtkNotebook-set-tab-detachable self child detachable)
"  ARGS: 
     child  - object Widget, 
     detachable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tab_detachable" child detachable)))

(define (GtkNotebook-set-tab-label self child tab-label)
"  ARGS: 
     child  - object Widget, 
     tab-label  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tab_label" child tab-label)))

(define (GtkNotebook-set-tab-label-text self child tab-text)
"  ARGS: 
     child  - object Widget, 
     tab-text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tab_label_text" child tab-text)))

(define (GtkNotebook-set-tab-pos self pos)
"  ARGS: 
     pos  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tab_pos" pos)))

(define (GtkNotebook-set-tab-reorderable self child reorderable)
"  ARGS: 
     child  - object Widget, 
     reorderable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tab_reorderable" child reorderable)))

(define <GtkNotebookAccessible>
  (gi-lookup-type "Gtk-NotebookAccessible"))

(define <GtkNotebookPageAccessible>
  (gi-lookup-type "Gtk-NotebookPageAccessible"))

(define (GtkNotebookPageAccessible-new notebook child)
"  ARGS: 
     notebook  - object NotebookAccessible, 
     child  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-NotebookPageAccessible-new" notebook child))

(define (GtkNotebookPageAccessible-invalidate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate")))

(define NOTEBOOK_TAB_FIRST
  (gi-enum-value "Gtk-NotebookTab" "first"))

(define NOTEBOOK_TAB_LAST
  (gi-enum-value "Gtk-NotebookTab" "last"))

(define NUMBER_UP_LAYOUT_LRTB
  (gi-enum-value "Gtk-NumberUpLayout" "lrtb"))

(define NUMBER_UP_LAYOUT_LRBT
  (gi-enum-value "Gtk-NumberUpLayout" "lrbt"))

(define NUMBER_UP_LAYOUT_RLTB
  (gi-enum-value "Gtk-NumberUpLayout" "rltb"))

(define NUMBER_UP_LAYOUT_RLBT
  (gi-enum-value "Gtk-NumberUpLayout" "rlbt"))

(define NUMBER_UP_LAYOUT_TBLR
  (gi-enum-value "Gtk-NumberUpLayout" "tblr"))

(define NUMBER_UP_LAYOUT_TBRL
  (gi-enum-value "Gtk-NumberUpLayout" "tbrl"))

(define NUMBER_UP_LAYOUT_BTLR
  (gi-enum-value "Gtk-NumberUpLayout" "btlr"))

(define NUMBER_UP_LAYOUT_BTRL
  (gi-enum-value "Gtk-NumberUpLayout" "btrl"))

(define <GtkNumerableIcon>
  (gi-lookup-type "Gtk-NumerableIcon"))

(define (GtkNumerableIcon-new base-icon)
"  ARGS: 
     base-icon  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-NumerableIcon-new" base-icon))

(define (GtkNumerableIcon-new-with-style-context base-icon context)
"  ARGS: 
     base-icon  - Unhandled argument type tag 16, 
     context  - object StyleContext
   RETURN: interface*
"
  (gi-function-invoke "Gtk-NumerableIcon-new_with_style_context" base-icon context))

(define (GtkNumerableIcon-get-background-gicon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_background_gicon")))

(define (GtkNumerableIcon-get-background-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_background_icon_name")))

(define (GtkNumerableIcon-get-count self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_count")))

(define (GtkNumerableIcon-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkNumerableIcon-get-style-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_style_context")))

(define (GtkNumerableIcon-set-background-gicon self icon)
"  ARGS: 
     icon  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background_gicon" icon)))

(define (GtkNumerableIcon-set-background-icon-name self icon-name)
"  ARGS: 
     icon-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background_icon_name" icon-name)))

(define (GtkNumerableIcon-set-count self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_count" count)))

(define (GtkNumerableIcon-set-label self label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkNumerableIcon-set-style-context self style)
"  ARGS: 
     style  - object StyleContext
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_style_context" style)))

(define <GtkOffscreenWindow>
  (gi-lookup-type "Gtk-OffscreenWindow"))

(define (GtkOffscreenWindow-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-OffscreenWindow-new"))

(define (GtkOffscreenWindow-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkOffscreenWindow-get-surface self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_surface")))

(define ORIENTATION_HORIZONTAL
  (gi-enum-value "Gtk-Orientation" "horizontal"))

(define ORIENTATION_VERTICAL
  (gi-enum-value "Gtk-Orientation" "vertical"))

(define <GtkOverlay>
  (gi-lookup-type "Gtk-Overlay"))

(define (GtkOverlay-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Overlay-new"))

(define (GtkOverlay-add-overlay self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_overlay" widget)))

(define (GtkOverlay-get-overlay-pass-through? self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_overlay_pass_through" widget)))

(define (GtkOverlay-reorder-overlay self child position)
"  ARGS: 
     child  - object Widget, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reorder_overlay" child position)))

(define (GtkOverlay-set-overlay-pass-through self widget pass-through)
"  ARGS: 
     widget  - object Widget, 
     pass-through  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_overlay_pass_through" widget pass-through)))

(define GTK_PAPER_NAME_A3
  (gi-constant-value "Gtk-PAPER_NAME_A3"))

(define GTK_PAPER_NAME_A4
  (gi-constant-value "Gtk-PAPER_NAME_A4"))

(define GTK_PAPER_NAME_A5
  (gi-constant-value "Gtk-PAPER_NAME_A5"))

(define GTK_PAPER_NAME_B5
  (gi-constant-value "Gtk-PAPER_NAME_B5"))

(define GTK_PAPER_NAME_EXECUTIVE
  (gi-constant-value "Gtk-PAPER_NAME_EXECUTIVE"))

(define GTK_PAPER_NAME_LEGAL
  (gi-constant-value "Gtk-PAPER_NAME_LEGAL"))

(define GTK_PAPER_NAME_LETTER
  (gi-constant-value "Gtk-PAPER_NAME_LETTER"))

(define GTK_PATH_PRIO_MASK
  (gi-constant-value "Gtk-PATH_PRIO_MASK"))

(define GTK_PRINT_SETTINGS_COLLATE
  (gi-constant-value "Gtk-PRINT_SETTINGS_COLLATE"))

(define GTK_PRINT_SETTINGS_DEFAULT_SOURCE
  (gi-constant-value "Gtk-PRINT_SETTINGS_DEFAULT_SOURCE"))

(define GTK_PRINT_SETTINGS_DITHER
  (gi-constant-value "Gtk-PRINT_SETTINGS_DITHER"))

(define GTK_PRINT_SETTINGS_DUPLEX
  (gi-constant-value "Gtk-PRINT_SETTINGS_DUPLEX"))

(define GTK_PRINT_SETTINGS_FINISHINGS
  (gi-constant-value "Gtk-PRINT_SETTINGS_FINISHINGS"))

(define GTK_PRINT_SETTINGS_MEDIA_TYPE
  (gi-constant-value "Gtk-PRINT_SETTINGS_MEDIA_TYPE"))

(define GTK_PRINT_SETTINGS_NUMBER_UP
  (gi-constant-value "Gtk-PRINT_SETTINGS_NUMBER_UP"))

(define GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
  (gi-constant-value "Gtk-PRINT_SETTINGS_NUMBER_UP_LAYOUT"))

(define GTK_PRINT_SETTINGS_N_COPIES
  (gi-constant-value "Gtk-PRINT_SETTINGS_N_COPIES"))

(define GTK_PRINT_SETTINGS_ORIENTATION
  (gi-constant-value "Gtk-PRINT_SETTINGS_ORIENTATION"))

(define GTK_PRINT_SETTINGS_OUTPUT_BASENAME
  (gi-constant-value "Gtk-PRINT_SETTINGS_OUTPUT_BASENAME"))

(define GTK_PRINT_SETTINGS_OUTPUT_BIN
  (gi-constant-value "Gtk-PRINT_SETTINGS_OUTPUT_BIN"))

(define GTK_PRINT_SETTINGS_OUTPUT_DIR
  (gi-constant-value "Gtk-PRINT_SETTINGS_OUTPUT_DIR"))

(define GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
  (gi-constant-value "Gtk-PRINT_SETTINGS_OUTPUT_FILE_FORMAT"))

(define GTK_PRINT_SETTINGS_OUTPUT_URI
  (gi-constant-value "Gtk-PRINT_SETTINGS_OUTPUT_URI"))

(define GTK_PRINT_SETTINGS_PAGE_RANGES
  (gi-constant-value "Gtk-PRINT_SETTINGS_PAGE_RANGES"))

(define GTK_PRINT_SETTINGS_PAGE_SET
  (gi-constant-value "Gtk-PRINT_SETTINGS_PAGE_SET"))

(define GTK_PRINT_SETTINGS_PAPER_FORMAT
  (gi-constant-value "Gtk-PRINT_SETTINGS_PAPER_FORMAT"))

(define GTK_PRINT_SETTINGS_PAPER_HEIGHT
  (gi-constant-value "Gtk-PRINT_SETTINGS_PAPER_HEIGHT"))

(define GTK_PRINT_SETTINGS_PAPER_WIDTH
  (gi-constant-value "Gtk-PRINT_SETTINGS_PAPER_WIDTH"))

(define GTK_PRINT_SETTINGS_PRINTER
  (gi-constant-value "Gtk-PRINT_SETTINGS_PRINTER"))

(define GTK_PRINT_SETTINGS_PRINTER_LPI
  (gi-constant-value "Gtk-PRINT_SETTINGS_PRINTER_LPI"))

(define GTK_PRINT_SETTINGS_PRINT_PAGES
  (gi-constant-value "Gtk-PRINT_SETTINGS_PRINT_PAGES"))

(define GTK_PRINT_SETTINGS_QUALITY
  (gi-constant-value "Gtk-PRINT_SETTINGS_QUALITY"))

(define GTK_PRINT_SETTINGS_RESOLUTION
  (gi-constant-value "Gtk-PRINT_SETTINGS_RESOLUTION"))

(define GTK_PRINT_SETTINGS_RESOLUTION_X
  (gi-constant-value "Gtk-PRINT_SETTINGS_RESOLUTION_X"))

(define GTK_PRINT_SETTINGS_RESOLUTION_Y
  (gi-constant-value "Gtk-PRINT_SETTINGS_RESOLUTION_Y"))

(define GTK_PRINT_SETTINGS_REVERSE
  (gi-constant-value "Gtk-PRINT_SETTINGS_REVERSE"))

(define GTK_PRINT_SETTINGS_SCALE
  (gi-constant-value "Gtk-PRINT_SETTINGS_SCALE"))

(define GTK_PRINT_SETTINGS_USE_COLOR
  (gi-constant-value "Gtk-PRINT_SETTINGS_USE_COLOR"))

(define GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
  (gi-constant-value "Gtk-PRINT_SETTINGS_WIN32_DRIVER_EXTRA"))

(define GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
  (gi-constant-value "Gtk-PRINT_SETTINGS_WIN32_DRIVER_VERSION"))

(define GTK_PRIORITY_RESIZE
  (gi-constant-value "Gtk-PRIORITY_RESIZE"))

(define PACK_DIRECTION_LTR
  (gi-enum-value "Gtk-PackDirection" "ltr"))

(define PACK_DIRECTION_RTL
  (gi-enum-value "Gtk-PackDirection" "rtl"))

(define PACK_DIRECTION_TTB
  (gi-enum-value "Gtk-PackDirection" "ttb"))

(define PACK_DIRECTION_BTT
  (gi-enum-value "Gtk-PackDirection" "btt"))

(define PACK_TYPE_START
  (gi-enum-value "Gtk-PackType" "start"))

(define PACK_TYPE_END
  (gi-enum-value "Gtk-PackType" "end"))

(define PAD_ACTION_TYPE_BUTTON
  (gi-enum-value "Gtk-PadActionType" "button"))

(define PAD_ACTION_TYPE_RING
  (gi-enum-value "Gtk-PadActionType" "ring"))

(define PAD_ACTION_TYPE_STRIP
  (gi-enum-value "Gtk-PadActionType" "strip"))

(define <GtkPadController>
  (gi-lookup-type "Gtk-PadController"))

(define (GtkPadController-new window group pad)
"  ARGS: 
     window  - object Window, 
     group  - Unhandled argument type tag 16, 
     pad  - object Device
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PadController-new" window group pad))

(define (GtkPadController-set-action self type index mode label action-name)
"  ARGS: 
     type  - exact integer of enum type PadActionType, 
     index  - exact integer of size gint32, 
     mode  - exact integer of size gint32, 
     label  - string, 
     action-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_action" type index mode label action-name)))

(define (GtkPadController-set-action-entries self entries n-entries)
"  ARGS: 
     entries  - Unhandled argument type tag 15, 
     n-entries  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_action_entries" entries n-entries)))

(define PAGE_ORIENTATION_PORTRAIT
  (gi-enum-value "Gtk-PageOrientation" "portrait"))

(define PAGE_ORIENTATION_LANDSCAPE
  (gi-enum-value "Gtk-PageOrientation" "landscape"))

(define PAGE_ORIENTATION_REVERSE_PORTRAIT
  (gi-enum-value "Gtk-PageOrientation" "reverse_portrait"))

(define PAGE_ORIENTATION_REVERSE_LANDSCAPE
  (gi-enum-value "Gtk-PageOrientation" "reverse_landscape"))

(define PAGE_SET_ALL
  (gi-enum-value "Gtk-PageSet" "all"))

(define PAGE_SET_EVEN
  (gi-enum-value "Gtk-PageSet" "even"))

(define PAGE_SET_ODD
  (gi-enum-value "Gtk-PageSet" "odd"))

(define <GtkPageSetup>
  (gi-lookup-type "Gtk-PageSetup"))

(define (GtkPageSetup-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PageSetup-new"))

(define (GtkPageSetup-new-from-file file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PageSetup-new_from_file" file-name))

(define (GtkPageSetup-new-from-gvariant variant)
"  ARGS: 
     variant  - struct Variant
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PageSetup-new_from_gvariant" variant))

(define (GtkPageSetup-new-from-key-file key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PageSetup-new_from_key_file" key-file group-name))

(define (GtkPageSetup-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkPageSetup-get-bottom-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_bottom_margin" unit)))

(define (GtkPageSetup-get-left-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_left_margin" unit)))

(define (GtkPageSetup-get-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_orientation")))

(define (GtkPageSetup-get-page-height self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_page_height" unit)))

(define (GtkPageSetup-get-page-width self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_page_width" unit)))

(define (GtkPageSetup-get-paper-height self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_height" unit)))

(define (GtkPageSetup-get-paper-size self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_size")))

(define (GtkPageSetup-get-paper-width self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_width" unit)))

(define (GtkPageSetup-get-right-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_right_margin" unit)))

(define (GtkPageSetup-get-top-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_top_margin" unit)))

(define (GtkPageSetup-load-file? self file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_file" file-name)))

(define (GtkPageSetup-load-key-file? self key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - #f for NULL or string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_key_file" key-file group-name)))

(define (GtkPageSetup-set-bottom-margin self margin unit)
"  ARGS: 
     margin  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_bottom_margin" margin unit)))

(define (GtkPageSetup-set-left-margin self margin unit)
"  ARGS: 
     margin  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_left_margin" margin unit)))

(define (GtkPageSetup-set-orientation self orientation)
"  ARGS: 
     orientation  - exact integer of enum type PageOrientation
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_orientation" orientation)))

(define (GtkPageSetup-set-paper-size self size)
"  ARGS: 
     size  - struct PaperSize
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_paper_size" size)))

(define (GtkPageSetup-set-paper-size-and-default-margins self size)
"  ARGS: 
     size  - struct PaperSize
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_paper_size_and_default_margins" size)))

(define (GtkPageSetup-set-right-margin self margin unit)
"  ARGS: 
     margin  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_right_margin" margin unit)))

(define (GtkPageSetup-set-top-margin self margin unit)
"  ARGS: 
     margin  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_top_margin" margin unit)))

(define (GtkPageSetup-to-file? self file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "to_file" file-name)))

(define (GtkPageSetup-to-gvariant self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "to_gvariant")))

(define (GtkPageSetup-to-key-file self key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "to_key_file" key-file group-name)))

;; CALLBACK
(define page-setup-done-func
  (gi-lookup-callback-info "Gtk-PageSetupDoneFunc"))
;; ARGS: 
;;   page-setup  - object PageSetup, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define PAN_DIRECTION_LEFT
  (gi-enum-value "Gtk-PanDirection" "left"))

(define PAN_DIRECTION_RIGHT
  (gi-enum-value "Gtk-PanDirection" "right"))

(define PAN_DIRECTION_UP
  (gi-enum-value "Gtk-PanDirection" "up"))

(define PAN_DIRECTION_DOWN
  (gi-enum-value "Gtk-PanDirection" "down"))

(define <GtkPaned>
  (gi-lookup-type "Gtk-Paned"))

(define (GtkPaned-new orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Paned-new" orientation))

(define (GtkPaned-add1 self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add1" child)))

(define (GtkPaned-add2 self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add2" child)))

(define (GtkPaned-get-child1 self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child1")))

(define (GtkPaned-get-child2 self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child2")))

(define (GtkPaned-get-handle-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_handle_window")))

(define (GtkPaned-get-position self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_position")))

(define (GtkPaned-get-wide-handle? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_wide_handle")))

(define (GtkPaned-pack1 self child resize shrink)
"  ARGS: 
     child  - object Widget, 
     resize  - boolean, 
     shrink  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack1" child resize shrink)))

(define (GtkPaned-pack2 self child resize shrink)
"  ARGS: 
     child  - object Widget, 
     resize  - boolean, 
     shrink  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack2" child resize shrink)))

(define (GtkPaned-set-position self position)
"  ARGS: 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_position" position)))

(define (GtkPaned-set-wide-handle self wide)
"  ARGS: 
     wide  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wide_handle" wide)))

(define <GtkPanedAccessible>
  (gi-lookup-type "Gtk-PanedAccessible"))

(define <GtkPaperSize>
  (gi-lookup-type "Gtk-PaperSize"))

(define (GtkPaperSize-new name)
"  ARGS: 
     name  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new" name))

(define (GtkPaperSize-new-custom name display-name width height unit)
"  ARGS: 
     name  - string, 
     display-name  - string, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new_custom" name display-name width height unit))

(define (GtkPaperSize-new-from-gvariant variant)
"  ARGS: 
     variant  - struct Variant
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new_from_gvariant" variant))

(define (GtkPaperSize-new-from-ipp ipp-name width height)
"  ARGS: 
     ipp-name  - string, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new_from_ipp" ipp-name width height))

(define (GtkPaperSize-new-from-key-file key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new_from_key_file" key-file group-name))

(define (GtkPaperSize-new-from-ppd ppd-name ppd-display-name width height)
"  ARGS: 
     ppd-name  - string, 
     ppd-display-name  - string, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PaperSize-new_from_ppd" ppd-name ppd-display-name width height))

(define (GtkPaperSize-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkPaperSize-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkPaperSize-get-default-bottom-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_default_bottom_margin" unit)))

(define (GtkPaperSize-get-default-left-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_default_left_margin" unit)))

(define (GtkPaperSize-get-default-right-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_default_right_margin" unit)))

(define (GtkPaperSize-get-default-top-margin self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_default_top_margin" unit)))

(define (GtkPaperSize-get-display-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_display_name")))

(define (GtkPaperSize-get-height self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_height" unit)))

(define (GtkPaperSize-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkPaperSize-get-ppd-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_ppd_name")))

(define (GtkPaperSize-get-width self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_width" unit)))

(define (GtkPaperSize-is-custom? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_custom")))

(define (GtkPaperSize-is-equal? self size2)
"  ARGS: 
     size2  - struct PaperSize
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_equal" size2)))

(define (GtkPaperSize-is-ipp? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_ipp")))

(define (GtkPaperSize-set-size self width height unit)
"  ARGS: 
     width  - real number of size gdouble, 
     height  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_size" width height unit)))

(define (GtkPaperSize-to-gvariant self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "to_gvariant")))

(define (GtkPaperSize-to-key-file self key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "to_key_file" key-file group-name)))

(define (GtkPaperSize-get-default)
"  ARGS: 
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-PaperSize-get_default"))

(define (GtkPaperSize-get-paper-sizes include-custom)
"  ARGS: 
     include-custom  - boolean
   RETURN: glist*
"
  (gi-function-invoke "Gtk-PaperSize-get_paper_sizes" include-custom))

(define NORMAL
  (gi-flag-value "Gtk-PlacesOpenFlags" "normal"))

(define NEW_TAB
  (gi-flag-value "Gtk-PlacesOpenFlags" "new_tab"))

(define NEW_WINDOW
  (gi-flag-value "Gtk-PlacesOpenFlags" "new_window"))

(define <GtkPlacesSidebar>
  (gi-lookup-type "Gtk-PlacesSidebar"))

(define (GtkPlacesSidebar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PlacesSidebar-new"))

(define (GtkPlacesSidebar-add-shortcut self location)
"  ARGS: 
     location  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_shortcut" location)))

(define (GtkPlacesSidebar-get-local-only? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_local_only")))

(define (GtkPlacesSidebar-get-location self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_location")))

(define (GtkPlacesSidebar-get-nth-bookmark self n)
"  ARGS: 
     n  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_nth_bookmark" n)))

(define (GtkPlacesSidebar-get-open-flags self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_open_flags")))

(define (GtkPlacesSidebar-get-show-connect-to-server? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_connect_to_server")))

(define (GtkPlacesSidebar-get-show-desktop? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_desktop")))

(define (GtkPlacesSidebar-get-show-enter-location? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_enter_location")))

(define (GtkPlacesSidebar-get-show-other-locations? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_other_locations")))

(define (GtkPlacesSidebar-get-show-recent? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_recent")))

(define (GtkPlacesSidebar-get-show-starred-location? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_starred_location")))

(define (GtkPlacesSidebar-get-show-trash? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_trash")))

(define (GtkPlacesSidebar-list-shortcuts self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "list_shortcuts")))

(define (GtkPlacesSidebar-remove-shortcut self location)
"  ARGS: 
     location  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_shortcut" location)))

(define (GtkPlacesSidebar-set-drop-targets-visible self visible context)
"  ARGS: 
     visible  - boolean, 
     context  - object DragContext
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_drop_targets_visible" visible context)))

(define (GtkPlacesSidebar-set-local-only self local-only)
"  ARGS: 
     local-only  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_local_only" local-only)))

(define (GtkPlacesSidebar-set-location self location)
"  ARGS: 
     location  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_location" location)))

(define (GtkPlacesSidebar-set-open-flags self flags)
"  ARGS: 
     flags  - exact integer of flags type PlacesOpenFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_open_flags" flags)))

(define (GtkPlacesSidebar-set-show-connect-to-server self show-connect-to-server)
"  ARGS: 
     show-connect-to-server  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_connect_to_server" show-connect-to-server)))

(define (GtkPlacesSidebar-set-show-desktop self show-desktop)
"  ARGS: 
     show-desktop  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_desktop" show-desktop)))

(define (GtkPlacesSidebar-set-show-enter-location self show-enter-location)
"  ARGS: 
     show-enter-location  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_enter_location" show-enter-location)))

(define (GtkPlacesSidebar-set-show-other-locations self show-other-locations)
"  ARGS: 
     show-other-locations  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_other_locations" show-other-locations)))

(define (GtkPlacesSidebar-set-show-recent self show-recent)
"  ARGS: 
     show-recent  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_recent" show-recent)))

(define (GtkPlacesSidebar-set-show-starred-location self show-starred-location)
"  ARGS: 
     show-starred-location  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_starred_location" show-starred-location)))

(define (GtkPlacesSidebar-set-show-trash self show-trash)
"  ARGS: 
     show-trash  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_trash" show-trash)))

(define POLICY_TYPE_ALWAYS
  (gi-enum-value "Gtk-PolicyType" "always"))

(define POLICY_TYPE_AUTOMATIC
  (gi-enum-value "Gtk-PolicyType" "automatic"))

(define POLICY_TYPE_NEVER
  (gi-enum-value "Gtk-PolicyType" "never"))

(define POLICY_TYPE_EXTERNAL
  (gi-enum-value "Gtk-PolicyType" "external"))

(define <GtkPopover>
  (gi-lookup-type "Gtk-Popover"))

(define (GtkPopover-new relative-to)
"  ARGS: 
     relative-to  - object Widget
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Popover-new" relative-to))

(define (GtkPopover-new-from-model relative-to model)
"  ARGS: 
     relative-to  - object Widget, 
     model  - object MenuModel
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Popover-new_from_model" relative-to model))

(define (GtkPopover-bind-model self model action-namespace)
"  ARGS: 
     model  - object MenuModel, 
     action-namespace  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "bind_model" model action-namespace)))

(define (GtkPopover-get-constrain-to self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_constrain_to")))

(define (GtkPopover-get-default-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_widget")))

(define (GtkPopover-get-modal? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_modal")))

(define (GtkPopover-get-pointing-to? self out-rect)
"  ARGS: 
   RETURN: gboolean
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_pointing_to" out-rect)))

(define (GtkPopover-get-position self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_position")))

(define (GtkPopover-get-relative-to self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_relative_to")))

(define (GtkPopover-get-transitions-enabled? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_transitions_enabled")))

(define (GtkPopover-popdown self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popdown")))

(define (GtkPopover-popup self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "popup")))

(define (GtkPopover-set-constrain-to self constraint)
"  ARGS: 
     constraint  - exact integer of enum type PopoverConstraint
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_constrain_to" constraint)))

(define (GtkPopover-set-default-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_widget" widget)))

(define (GtkPopover-set-modal self modal)
"  ARGS: 
     modal  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_modal" modal)))

(define (GtkPopover-set-pointing-to self rect)
"  ARGS: 
     rect  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pointing_to" rect)))

(define (GtkPopover-set-position self position)
"  ARGS: 
     position  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_position" position)))

(define (GtkPopover-set-relative-to self relative-to)
"  ARGS: 
     relative-to  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_relative_to" relative-to)))

(define (GtkPopover-set-transitions-enabled self transitions-enabled)
"  ARGS: 
     transitions-enabled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transitions_enabled" transitions-enabled)))

(define <GtkPopoverAccessible>
  (gi-lookup-type "Gtk-PopoverAccessible"))

(define POPOVER_CONSTRAINT_NONE
  (gi-enum-value "Gtk-PopoverConstraint" "none"))

(define POPOVER_CONSTRAINT_WINDOW
  (gi-enum-value "Gtk-PopoverConstraint" "window"))

(define <GtkPopoverMenu>
  (gi-lookup-type "Gtk-PopoverMenu"))

(define (GtkPopoverMenu-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PopoverMenu-new"))

(define (GtkPopoverMenu-open-submenu self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "open_submenu" name)))

(define POSITION_TYPE_LEFT
  (gi-enum-value "Gtk-PositionType" "left"))

(define POSITION_TYPE_RIGHT
  (gi-enum-value "Gtk-PositionType" "right"))

(define POSITION_TYPE_TOP
  (gi-enum-value "Gtk-PositionType" "top"))

(define POSITION_TYPE_BOTTOM
  (gi-enum-value "Gtk-PositionType" "bottom"))

(define <GtkPrintContext>
  (gi-lookup-type "Gtk-PrintContext"))

(define (GtkPrintContext-create-pango-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_pango_context")))

(define (GtkPrintContext-create-pango-layout self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_pango_layout")))

(define (GtkPrintContext-get-cairo-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_cairo_context")))

(define (GtkPrintContext-get-dpi-x self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_dpi_x")))

(define (GtkPrintContext-get-dpi-y self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_dpi_y")))

(define (GtkPrintContext-get-hard-margins? self)
"  ARGS: 
     top  - real number of size gdouble[OUT], 
     bottom  - real number of size gdouble[OUT], 
     left  - real number of size gdouble[OUT], 
     right  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hard_margins")))

(define (GtkPrintContext-get-height self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_height")))

(define (GtkPrintContext-get-page-setup self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_page_setup")))

(define (GtkPrintContext-get-pango-fontmap self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pango_fontmap")))

(define (GtkPrintContext-get-width self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_width")))

(define (GtkPrintContext-set-cairo-context self cr dpi-x dpi-y)
"  ARGS: 
     cr  - struct Context, 
     dpi-x  - real number of size gdouble, 
     dpi-y  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cairo_context" cr dpi-x dpi-y)))

(define PRINT_DUPLEX_SIMPLEX
  (gi-enum-value "Gtk-PrintDuplex" "simplex"))

(define PRINT_DUPLEX_HORIZONTAL
  (gi-enum-value "Gtk-PrintDuplex" "horizontal"))

(define PRINT_DUPLEX_VERTICAL
  (gi-enum-value "Gtk-PrintDuplex" "vertical"))

(define PRINT_ERROR_GENERAL
  (gi-enum-value "Gtk-PrintError" "general"))

(define PRINT_ERROR_INTERNAL_ERROR
  (gi-enum-value "Gtk-PrintError" "internal_error"))

(define PRINT_ERROR_NOMEM
  (gi-enum-value "Gtk-PrintError" "nomem"))

(define PRINT_ERROR_INVALID_FILE
  (gi-enum-value "Gtk-PrintError" "invalid_file"))

(define <GtkPrintOperation>
  (gi-lookup-type "Gtk-PrintOperation"))

(define (GtkPrintOperation-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PrintOperation-new"))

(define (GtkPrintOperation-cancel self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cancel")))

(define (GtkPrintOperation-draw-page-finish self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "draw_page_finish")))

(define (GtkPrintOperation-get-default-page-setup self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_page_setup")))

(define (GtkPrintOperation-get-embed-page-setup? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_embed_page_setup")))

(define (GtkPrintOperation-get-error self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_error")))

(define (GtkPrintOperation-get-has-selection? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_selection")))

(define (GtkPrintOperation-get-n-pages-to-print self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_pages_to_print")))

(define (GtkPrintOperation-get-print-settings self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_print_settings")))

(define (GtkPrintOperation-get-status self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_status")))

(define (GtkPrintOperation-get-status-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_status_string")))

(define (GtkPrintOperation-get-support-selection? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_support_selection")))

(define (GtkPrintOperation-is-finished? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_finished")))

(define (GtkPrintOperation-run self action parent)
"  ARGS: 
     action  - exact integer of enum type PrintOperationAction, 
     parent  - object Window
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "run" action parent)))

(define (GtkPrintOperation-set-allow-async self allow-async)
"  ARGS: 
     allow-async  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_allow_async" allow-async)))

(define (GtkPrintOperation-set-current-page self current-page)
"  ARGS: 
     current-page  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_page" current-page)))

(define (GtkPrintOperation-set-custom-tab-label self label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_custom_tab_label" label)))

(define (GtkPrintOperation-set-default-page-setup self default-page-setup)
"  ARGS: 
     default-page-setup  - object PageSetup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_page_setup" default-page-setup)))

(define (GtkPrintOperation-set-defer-drawing self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_defer_drawing")))

(define (GtkPrintOperation-set-embed-page-setup self embed)
"  ARGS: 
     embed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_embed_page_setup" embed)))

(define (GtkPrintOperation-set-export-filename self filename)
"  ARGS: 
     filename  - locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_export_filename" filename)))

(define (GtkPrintOperation-set-has-selection self has-selection)
"  ARGS: 
     has-selection  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_selection" has-selection)))

(define (GtkPrintOperation-set-job-name self job-name)
"  ARGS: 
     job-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_job_name" job-name)))

(define (GtkPrintOperation-set-n-pages self n-pages)
"  ARGS: 
     n-pages  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_n_pages" n-pages)))

(define (GtkPrintOperation-set-print-settings self print-settings)
"  ARGS: 
     print-settings  - object PrintSettings
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_print_settings" print-settings)))

(define (GtkPrintOperation-set-show-progress self show-progress)
"  ARGS: 
     show-progress  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_progress" show-progress)))

(define (GtkPrintOperation-set-support-selection self support-selection)
"  ARGS: 
     support-selection  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_support_selection" support-selection)))

(define (GtkPrintOperation-set-track-print-status self track-status)
"  ARGS: 
     track-status  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_track_print_status" track-status)))

(define (GtkPrintOperation-set-unit self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_unit" unit)))

(define (GtkPrintOperation-set-use-full-page self full-page)
"  ARGS: 
     full-page  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_full_page" full-page)))

(define PRINT_OPERATION_ACTION_PRINT_DIALOG
  (gi-enum-value "Gtk-PrintOperationAction" "print_dialog"))

(define PRINT_OPERATION_ACTION_PRINT
  (gi-enum-value "Gtk-PrintOperationAction" "print"))

(define PRINT_OPERATION_ACTION_PREVIEW
  (gi-enum-value "Gtk-PrintOperationAction" "preview"))

(define PRINT_OPERATION_ACTION_EXPORT
  (gi-enum-value "Gtk-PrintOperationAction" "export"))

(define PRINT_OPERATION_RESULT_ERROR
  (gi-enum-value "Gtk-PrintOperationResult" "error"))

(define PRINT_OPERATION_RESULT_APPLY
  (gi-enum-value "Gtk-PrintOperationResult" "apply"))

(define PRINT_OPERATION_RESULT_CANCEL
  (gi-enum-value "Gtk-PrintOperationResult" "cancel"))

(define PRINT_OPERATION_RESULT_IN_PROGRESS
  (gi-enum-value "Gtk-PrintOperationResult" "in_progress"))

(define PRINT_PAGES_ALL
  (gi-enum-value "Gtk-PrintPages" "all"))

(define PRINT_PAGES_CURRENT
  (gi-enum-value "Gtk-PrintPages" "current"))

(define PRINT_PAGES_RANGES
  (gi-enum-value "Gtk-PrintPages" "ranges"))

(define PRINT_PAGES_SELECTION
  (gi-enum-value "Gtk-PrintPages" "selection"))

(define PRINT_QUALITY_LOW
  (gi-enum-value "Gtk-PrintQuality" "low"))

(define PRINT_QUALITY_NORMAL
  (gi-enum-value "Gtk-PrintQuality" "normal"))

(define PRINT_QUALITY_HIGH
  (gi-enum-value "Gtk-PrintQuality" "high"))

(define PRINT_QUALITY_DRAFT
  (gi-enum-value "Gtk-PrintQuality" "draft"))

(define <GtkPrintSettings>
  (gi-lookup-type "Gtk-PrintSettings"))

(define (GtkPrintSettings-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PrintSettings-new"))

(define (GtkPrintSettings-new-from-file file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PrintSettings-new_from_file" file-name))

(define (GtkPrintSettings-new-from-gvariant variant)
"  ARGS: 
     variant  - struct Variant
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PrintSettings-new_from_gvariant" variant))

(define (GtkPrintSettings-new-from-key-file key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-PrintSettings-new_from_key_file" key-file group-name))

(define (GtkPrintSettings-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkPrintSettings-foreach self func user-data)
"  ARGS: 
     func  - procedure of type PrintSettingsFunc, 
     user-data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "foreach" func user-data)))

(define (GtkPrintSettings-get self key)
"  ARGS: 
     key  - string
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get" key)))

(define (GtkPrintSettings-get-bool? self key)
"  ARGS: 
     key  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_bool" key)))

(define (GtkPrintSettings-get-collate? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_collate")))

(define (GtkPrintSettings-get-default-source self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_source")))

(define (GtkPrintSettings-get-dither self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_dither")))

(define (GtkPrintSettings-get-double self key)
"  ARGS: 
     key  - string
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_double" key)))

(define (GtkPrintSettings-get-double-with-default self key def)
"  ARGS: 
     key  - string, 
     def  - real number of size gdouble
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_double_with_default" key def)))

(define (GtkPrintSettings-get-duplex self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_duplex")))

(define (GtkPrintSettings-get-finishings self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_finishings")))

(define (GtkPrintSettings-get-int self key)
"  ARGS: 
     key  - string
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_int" key)))

(define (GtkPrintSettings-get-int-with-default self key def)
"  ARGS: 
     key  - string, 
     def  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_int_with_default" key def)))

(define (GtkPrintSettings-get-length self key unit)
"  ARGS: 
     key  - string, 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_length" key unit)))

(define (GtkPrintSettings-get-media-type self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_media_type")))

(define (GtkPrintSettings-get-n-copies self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_copies")))

(define (GtkPrintSettings-get-number-up self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_number_up")))

(define (GtkPrintSettings-get-number-up-layout self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_number_up_layout")))

(define (GtkPrintSettings-get-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_orientation")))

(define (GtkPrintSettings-get-output-bin self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_output_bin")))

(define (GtkPrintSettings-get-page-ranges self)
"  ARGS: 
     num-ranges  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_page_ranges")))

(define (GtkPrintSettings-get-page-set self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_page_set")))

(define (GtkPrintSettings-get-paper-height self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_height" unit)))

(define (GtkPrintSettings-get-paper-size self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_size")))

(define (GtkPrintSettings-get-paper-width self unit)
"  ARGS: 
     unit  - exact integer of enum type Unit
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_paper_width" unit)))

(define (GtkPrintSettings-get-print-pages self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_print_pages")))

(define (GtkPrintSettings-get-printer self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_printer")))

(define (GtkPrintSettings-get-printer-lpi self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_printer_lpi")))

(define (GtkPrintSettings-get-quality self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_quality")))

(define (GtkPrintSettings-get-resolution self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_resolution")))

(define (GtkPrintSettings-get-resolution-x self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_resolution_x")))

(define (GtkPrintSettings-get-resolution-y self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_resolution_y")))

(define (GtkPrintSettings-get-reverse? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reverse")))

(define (GtkPrintSettings-get-scale self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_scale")))

(define (GtkPrintSettings-get-use-color? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_color")))

(define (GtkPrintSettings-has-key? self key)
"  ARGS: 
     key  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_key" key)))

(define (GtkPrintSettings-load-file? self file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_file" file-name)))

(define (GtkPrintSettings-load-key-file? self key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - #f for NULL or string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "load_key_file" key-file group-name)))

(define (GtkPrintSettings-set self key value)
"  ARGS: 
     key  - string, 
     value  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" key value)))

(define (GtkPrintSettings-set-bool self key value)
"  ARGS: 
     key  - string, 
     value  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_bool" key value)))

(define (GtkPrintSettings-set-collate self collate)
"  ARGS: 
     collate  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_collate" collate)))

(define (GtkPrintSettings-set-default-source self default-source)
"  ARGS: 
     default-source  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_source" default-source)))

(define (GtkPrintSettings-set-dither self dither)
"  ARGS: 
     dither  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_dither" dither)))

(define (GtkPrintSettings-set-double self key value)
"  ARGS: 
     key  - string, 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_double" key value)))

(define (GtkPrintSettings-set-duplex self duplex)
"  ARGS: 
     duplex  - exact integer of enum type PrintDuplex
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_duplex" duplex)))

(define (GtkPrintSettings-set-finishings self finishings)
"  ARGS: 
     finishings  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_finishings" finishings)))

(define (GtkPrintSettings-set-int self key value)
"  ARGS: 
     key  - string, 
     value  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_int" key value)))

(define (GtkPrintSettings-set-length self key value unit)
"  ARGS: 
     key  - string, 
     value  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_length" key value unit)))

(define (GtkPrintSettings-set-media-type self media-type)
"  ARGS: 
     media-type  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_media_type" media-type)))

(define (GtkPrintSettings-set-n-copies self num-copies)
"  ARGS: 
     num-copies  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_n_copies" num-copies)))

(define (GtkPrintSettings-set-number-up self number-up)
"  ARGS: 
     number-up  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_number_up" number-up)))

(define (GtkPrintSettings-set-number-up-layout self number-up-layout)
"  ARGS: 
     number-up-layout  - exact integer of enum type NumberUpLayout
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_number_up_layout" number-up-layout)))

(define (GtkPrintSettings-set-orientation self orientation)
"  ARGS: 
     orientation  - exact integer of enum type PageOrientation
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_orientation" orientation)))

(define (GtkPrintSettings-set-output-bin self output-bin)
"  ARGS: 
     output-bin  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_output_bin" output-bin)))

(define (GtkPrintSettings-set-page-ranges self page-ranges num-ranges)
"  ARGS: 
     page-ranges  - Unhandled argument type tag 15, 
     num-ranges  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_ranges" page-ranges num-ranges)))

(define (GtkPrintSettings-set-page-set self page-set)
"  ARGS: 
     page-set  - exact integer of enum type PageSet
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_page_set" page-set)))

(define (GtkPrintSettings-set-paper-height self height unit)
"  ARGS: 
     height  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_paper_height" height unit)))

(define (GtkPrintSettings-set-paper-size self paper-size)
"  ARGS: 
     paper-size  - struct PaperSize
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_paper_size" paper-size)))

(define (GtkPrintSettings-set-paper-width self width unit)
"  ARGS: 
     width  - real number of size gdouble, 
     unit  - exact integer of enum type Unit
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_paper_width" width unit)))

(define (GtkPrintSettings-set-print-pages self pages)
"  ARGS: 
     pages  - exact integer of enum type PrintPages
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_print_pages" pages)))

(define (GtkPrintSettings-set-printer self printer)
"  ARGS: 
     printer  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_printer" printer)))

(define (GtkPrintSettings-set-printer-lpi self lpi)
"  ARGS: 
     lpi  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_printer_lpi" lpi)))

(define (GtkPrintSettings-set-quality self quality)
"  ARGS: 
     quality  - exact integer of enum type PrintQuality
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_quality" quality)))

(define (GtkPrintSettings-set-resolution self resolution)
"  ARGS: 
     resolution  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resolution" resolution)))

(define (GtkPrintSettings-set-resolution-xy self resolution-x resolution-y)
"  ARGS: 
     resolution-x  - exact integer of size gint32, 
     resolution-y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resolution_xy" resolution-x resolution-y)))

(define (GtkPrintSettings-set-reverse self reverse)
"  ARGS: 
     reverse  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reverse" reverse)))

(define (GtkPrintSettings-set-scale self scale)
"  ARGS: 
     scale  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_scale" scale)))

(define (GtkPrintSettings-set-use-color self use-color)
"  ARGS: 
     use-color  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_color" use-color)))

(define (GtkPrintSettings-to-file? self file-name)
"  ARGS: 
     file-name  - locale string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "to_file" file-name)))

(define (GtkPrintSettings-to-gvariant self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "to_gvariant")))

(define (GtkPrintSettings-to-key-file self key-file group-name)
"  ARGS: 
     key-file  - struct KeyFile, 
     group-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "to_key_file" key-file group-name)))

(define (GtkPrintSettings-unset self key)
"  ARGS: 
     key  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset" key)))

;; CALLBACK
(define print-settings-func
  (gi-lookup-callback-info "Gtk-PrintSettingsFunc"))
;; ARGS: 
;;   key  - string, 
;;   value  - string, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define PRINT_STATUS_INITIAL
  (gi-enum-value "Gtk-PrintStatus" "initial"))

(define PRINT_STATUS_PREPARING
  (gi-enum-value "Gtk-PrintStatus" "preparing"))

(define PRINT_STATUS_GENERATING_DATA
  (gi-enum-value "Gtk-PrintStatus" "generating_data"))

(define PRINT_STATUS_SENDING_DATA
  (gi-enum-value "Gtk-PrintStatus" "sending_data"))

(define PRINT_STATUS_PENDING
  (gi-enum-value "Gtk-PrintStatus" "pending"))

(define PRINT_STATUS_PENDING_ISSUE
  (gi-enum-value "Gtk-PrintStatus" "pending_issue"))

(define PRINT_STATUS_PRINTING
  (gi-enum-value "Gtk-PrintStatus" "printing"))

(define PRINT_STATUS_FINISHED
  (gi-enum-value "Gtk-PrintStatus" "finished"))

(define PRINT_STATUS_FINISHED_ABORTED
  (gi-enum-value "Gtk-PrintStatus" "finished_aborted"))

(define <GtkProgressBar>
  (gi-lookup-type "Gtk-ProgressBar"))

(define (GtkProgressBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ProgressBar-new"))

(define (GtkProgressBar-get-ellipsize self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_ellipsize")))

(define (GtkProgressBar-get-fraction self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_fraction")))

(define (GtkProgressBar-get-inverted? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inverted")))

(define (GtkProgressBar-get-pulse-step self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_pulse_step")))

(define (GtkProgressBar-get-show-text? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_text")))

(define (GtkProgressBar-get-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text")))

(define (GtkProgressBar-pulse self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pulse")))

(define (GtkProgressBar-set-ellipsize self mode)
"  ARGS: 
     mode  - exact integer of enum type EllipsizeMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_ellipsize" mode)))

(define (GtkProgressBar-set-fraction self fraction)
"  ARGS: 
     fraction  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fraction" fraction)))

(define (GtkProgressBar-set-inverted self inverted)
"  ARGS: 
     inverted  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inverted" inverted)))

(define (GtkProgressBar-set-pulse-step self fraction)
"  ARGS: 
     fraction  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pulse_step" fraction)))

(define (GtkProgressBar-set-show-text self show-text)
"  ARGS: 
     show-text  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_text" show-text)))

(define (GtkProgressBar-set-text self text)
"  ARGS: 
     text  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" text)))

(define <GtkProgressBarAccessible>
  (gi-lookup-type "Gtk-ProgressBarAccessible"))

(define PROPAGATION_PHASE_NONE
  (gi-enum-value "Gtk-PropagationPhase" "none"))

(define PROPAGATION_PHASE_CAPTURE
  (gi-enum-value "Gtk-PropagationPhase" "capture"))

(define PROPAGATION_PHASE_BUBBLE
  (gi-enum-value "Gtk-PropagationPhase" "bubble"))

(define PROPAGATION_PHASE_TARGET
  (gi-enum-value "Gtk-PropagationPhase" "target"))

(define <GtkRadioAction>
  (gi-lookup-type "Gtk-RadioAction"))

(define (GtkRadioAction-new name label tooltip stock-id value)
"  ARGS: 
     name  - string, 
     label  - #f for NULL or string, 
     tooltip  - #f for NULL or string, 
     stock-id  - #f for NULL or string, 
     value  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioAction-new" name label tooltip stock-id value))

(define (GtkRadioAction-get-current-value self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_current_value")))

(define (GtkRadioAction-get-group self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkRadioAction-join-group self group-source)
"  ARGS: 
     group-source  - object RadioAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "join_group" group-source)))

(define (GtkRadioAction-set-current-value self current-value)
"  ARGS: 
     current-value  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_current_value" current-value)))

(define (GtkRadioAction-set-group self group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group" group)))

(define <GtkRadioButton>
  (gi-lookup-type "Gtk-RadioButton"))

(define (GtkRadioButton-new group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new" group))

(define (GtkRadioButton-new-from-widget radio-group-member)
"  ARGS: 
     radio-group-member  - object RadioButton
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new_from_widget" radio-group-member))

(define (GtkRadioButton-new-with-label group label)
"  ARGS: 
     group  - #f for NULL or <GSList>, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new_with_label" group label))

(define (GtkRadioButton-new-with-label-from-widget radio-group-member label)
"  ARGS: 
     radio-group-member  - object RadioButton, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new_with_label_from_widget" radio-group-member label))

(define (GtkRadioButton-new-with-mnemonic group label)
"  ARGS: 
     group  - #f for NULL or <GSList>, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new_with_mnemonic" group label))

(define (GtkRadioButton-new-with-mnemonic-from-widget radio-group-member label)
"  ARGS: 
     radio-group-member  - object RadioButton, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioButton-new_with_mnemonic_from_widget" radio-group-member label))

(define (GtkRadioButton-get-group self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkRadioButton-join-group self group-source)
"  ARGS: 
     group-source  - object RadioButton
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "join_group" group-source)))

(define (GtkRadioButton-set-group self group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group" group)))

(define <GtkRadioButtonAccessible>
  (gi-lookup-type "Gtk-RadioButtonAccessible"))

(define <GtkRadioMenuItem>
  (gi-lookup-type "Gtk-RadioMenuItem"))

(define (GtkRadioMenuItem-new group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new" group))

(define (GtkRadioMenuItem-new-from-widget group)
"  ARGS: 
     group  - object RadioMenuItem
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new_from_widget" group))

(define (GtkRadioMenuItem-new-with-label group label)
"  ARGS: 
     group  - #f for NULL or <GSList>, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new_with_label" group label))

(define (GtkRadioMenuItem-new-with-label-from-widget group label)
"  ARGS: 
     group  - object RadioMenuItem, 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new_with_label_from_widget" group label))

(define (GtkRadioMenuItem-new-with-mnemonic group label)
"  ARGS: 
     group  - #f for NULL or <GSList>, 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new_with_mnemonic" group label))

(define (GtkRadioMenuItem-new-with-mnemonic-from-widget group label)
"  ARGS: 
     group  - object RadioMenuItem, 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioMenuItem-new_with_mnemonic_from_widget" group label))

(define (GtkRadioMenuItem-get-group self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkRadioMenuItem-join-group self group-source)
"  ARGS: 
     group-source  - object RadioMenuItem
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "join_group" group-source)))

(define (GtkRadioMenuItem-set-group self group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group" group)))

(define <GtkRadioMenuItemAccessible>
  (gi-lookup-type "Gtk-RadioMenuItemAccessible"))

(define <GtkRadioToolButton>
  (gi-lookup-type "Gtk-RadioToolButton"))

(define (GtkRadioToolButton-new group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioToolButton-new" group))

(define (GtkRadioToolButton-new-from-stock group stock-id)
"  ARGS: 
     group  - #f for NULL or <GSList>, 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioToolButton-new_from_stock" group stock-id))

(define (GtkRadioToolButton-new-from-widget group)
"  ARGS: 
     group  - object RadioToolButton
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioToolButton-new_from_widget" group))

(define (GtkRadioToolButton-new-with-stock-from-widget group stock-id)
"  ARGS: 
     group  - object RadioToolButton, 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RadioToolButton-new_with_stock_from_widget" group stock-id))

(define (GtkRadioToolButton-get-group self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkRadioToolButton-set-group self group)
"  ARGS: 
     group  - #f for NULL or <GSList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group" group)))

(define <GtkRange>
  (gi-lookup-type "Gtk-Range"))

(define (GtkRange-get-adjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_adjustment")))

(define (GtkRange-get-fill-level self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_fill_level")))

(define (GtkRange-get-flippable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_flippable")))

(define (GtkRange-get-inverted? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inverted")))

(define (GtkRange-get-lower-stepper-sensitivity self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_lower_stepper_sensitivity")))

(define (GtkRange-get-min-slider-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_min_slider_size")))

(define (GtkRange-get-range-rect self out-range-rect)
"  ARGS: 
   RETURN: void
     range-rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_range_rect" out-range-rect)))

(define (GtkRange-get-restrict-to-fill-level? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_restrict_to_fill_level")))

(define (GtkRange-get-round-digits self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_round_digits")))

(define (GtkRange-get-show-fill-level? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_fill_level")))

(define (GtkRange-get-slider-range self)
"  ARGS: 
     slider-start  - exact integer of size gint32[OUT], 
     slider-end  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_slider_range")))

(define (GtkRange-get-slider-size-fixed? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_slider_size_fixed")))

(define (GtkRange-get-upper-stepper-sensitivity self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_upper_stepper_sensitivity")))

(define (GtkRange-get-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_value")))

(define (GtkRange-set-adjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_adjustment" adjustment)))

(define (GtkRange-set-fill-level self fill-level)
"  ARGS: 
     fill-level  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fill_level" fill-level)))

(define (GtkRange-set-flippable self flippable)
"  ARGS: 
     flippable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_flippable" flippable)))

(define (GtkRange-set-increments self step page)
"  ARGS: 
     step  - real number of size gdouble, 
     page  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_increments" step page)))

(define (GtkRange-set-inverted self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inverted" setting)))

(define (GtkRange-set-lower-stepper-sensitivity self sensitivity)
"  ARGS: 
     sensitivity  - exact integer of enum type SensitivityType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_lower_stepper_sensitivity" sensitivity)))

(define (GtkRange-set-min-slider-size self min-size)
"  ARGS: 
     min-size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_slider_size" min-size)))

(define (GtkRange-set-range self min max)
"  ARGS: 
     min  - real number of size gdouble, 
     max  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_range" min max)))

(define (GtkRange-set-restrict-to-fill-level self restrict-to-fill-level)
"  ARGS: 
     restrict-to-fill-level  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_restrict_to_fill_level" restrict-to-fill-level)))

(define (GtkRange-set-round-digits self round-digits)
"  ARGS: 
     round-digits  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_round_digits" round-digits)))

(define (GtkRange-set-show-fill-level self show-fill-level)
"  ARGS: 
     show-fill-level  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_fill_level" show-fill-level)))

(define (GtkRange-set-slider-size-fixed self size-fixed)
"  ARGS: 
     size-fixed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_slider_size_fixed" size-fixed)))

(define (GtkRange-set-upper-stepper-sensitivity self sensitivity)
"  ARGS: 
     sensitivity  - exact integer of enum type SensitivityType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_upper_stepper_sensitivity" sensitivity)))

(define (GtkRange-set-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" value)))

(define <GtkRangeAccessible>
  (gi-lookup-type "Gtk-RangeAccessible"))

(define FG
  (gi-flag-value "Gtk-RcFlags" "fg"))

(define BG
  (gi-flag-value "Gtk-RcFlags" "bg"))

(define TEXT
  (gi-flag-value "Gtk-RcFlags" "text"))

(define BASE
  (gi-flag-value "Gtk-RcFlags" "base"))

;; CALLBACK
(define rc-property-parser
  (gi-lookup-callback-info "Gtk-RcPropertyParser"))
;; ARGS: 
;;   pspec  - object ParamSpec, 
;;   rc-string  - struct String, 
;;   property-value  - struct Value
;; RETURN: gboolean
(define <GtkRcStyle>
  (gi-lookup-type "Gtk-RcStyle"))

(define (GtkRcStyle-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RcStyle-new"))

(define (GtkRcStyle-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define <GtkRecentAction>
  (gi-lookup-type "Gtk-RecentAction"))

(define (GtkRecentAction-new name label tooltip stock-id)
"  ARGS: 
     name  - string, 
     label  - #f for NULL or string, 
     tooltip  - #f for NULL or string, 
     stock-id  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentAction-new" name label tooltip stock-id))

(define (GtkRecentAction-new-for-manager name label tooltip stock-id manager)
"  ARGS: 
     name  - string, 
     label  - #f for NULL or string, 
     tooltip  - #f for NULL or string, 
     stock-id  - #f for NULL or string, 
     manager  - object RecentManager
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentAction-new_for_manager" name label tooltip stock-id manager))

(define (GtkRecentAction-get-show-numbers? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_numbers")))

(define (GtkRecentAction-set-show-numbers self show-numbers)
"  ARGS: 
     show-numbers  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_numbers" show-numbers)))

(define <GtkRecentChooserDialog>
  (gi-lookup-type "Gtk-RecentChooserDialog"))

(define RECENT_CHOOSER_ERROR_NOT_FOUND
  (gi-enum-value "Gtk-RecentChooserError" "not_found"))

(define RECENT_CHOOSER_ERROR_INVALID_URI
  (gi-enum-value "Gtk-RecentChooserError" "invalid_uri"))

(define <GtkRecentChooserMenu>
  (gi-lookup-type "Gtk-RecentChooserMenu"))

(define (GtkRecentChooserMenu-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentChooserMenu-new"))

(define (GtkRecentChooserMenu-new-for-manager manager)
"  ARGS: 
     manager  - object RecentManager
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentChooserMenu-new_for_manager" manager))

(define (GtkRecentChooserMenu-get-show-numbers? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_numbers")))

(define (GtkRecentChooserMenu-set-show-numbers self show-numbers)
"  ARGS: 
     show-numbers  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_numbers" show-numbers)))

(define <GtkRecentChooserWidget>
  (gi-lookup-type "Gtk-RecentChooserWidget"))

(define (GtkRecentChooserWidget-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentChooserWidget-new"))

(define (GtkRecentChooserWidget-new-for-manager manager)
"  ARGS: 
     manager  - object RecentManager
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentChooserWidget-new_for_manager" manager))

(define <GtkRecentFilter>
  (gi-lookup-type "Gtk-RecentFilter"))

(define (GtkRecentFilter-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentFilter-new"))

(define (GtkRecentFilter-add-age self days)
"  ARGS: 
     days  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_age" days)))

(define (GtkRecentFilter-add-application self application)
"  ARGS: 
     application  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_application" application)))

(define (GtkRecentFilter-add-custom self needed func data data-destroy)
"  ARGS: 
     needed  - exact integer of flags type RecentFilterFlags, 
     func  - procedure of type RecentFilterFunc, 
     data  - #f for NULL or pointer, 
     data-destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_custom" needed func data data-destroy)))

(define (GtkRecentFilter-add-group self group)
"  ARGS: 
     group  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_group" group)))

(define (GtkRecentFilter-add-mime-type self mime-type)
"  ARGS: 
     mime-type  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mime_type" mime-type)))

(define (GtkRecentFilter-add-pattern self pattern)
"  ARGS: 
     pattern  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_pattern" pattern)))

(define (GtkRecentFilter-add-pixbuf-formats self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_pixbuf_formats")))

(define (GtkRecentFilter-filter? self filter-info)
"  ARGS: 
     filter-info  - struct RecentFilterInfo
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "filter" filter-info)))

(define (GtkRecentFilter-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkRecentFilter-get-needed self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_needed")))

(define (GtkRecentFilter-set-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_name" name)))

(define URI
  (gi-flag-value "Gtk-RecentFilterFlags" "uri"))

(define DISPLAY_NAME
  (gi-flag-value "Gtk-RecentFilterFlags" "display_name"))

(define MIME_TYPE
  (gi-flag-value "Gtk-RecentFilterFlags" "mime_type"))

(define APPLICATION
  (gi-flag-value "Gtk-RecentFilterFlags" "application"))

(define GROUP
  (gi-flag-value "Gtk-RecentFilterFlags" "group"))

(define AGE
  (gi-flag-value "Gtk-RecentFilterFlags" "age"))

;; CALLBACK
(define recent-filter-func
  (gi-lookup-callback-info "Gtk-RecentFilterFunc"))
;; ARGS: 
;;   filter-info  - struct RecentFilterInfo, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkRecentInfo>
  (gi-lookup-type "Gtk-RecentInfo"))

(define (GtkRecentInfo-create-app-info self app-name)
"  ARGS: 
     app-name  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_app_info" app-name)))

(define (GtkRecentInfo-exists? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "exists")))

(define (GtkRecentInfo-get-added self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_added")))

(define (GtkRecentInfo-get-age self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_age")))

(define (GtkRecentInfo-get-application-info? self app-name)
"  ARGS: 
     app-name  - string, 
     app-exec  - string[OUT], 
     count  - exact integer of size guint32[OUT], 
     time-  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_application_info" app-name)))

(define (GtkRecentInfo-get-applications self)
"  ARGS: 
     length  - exact integer of size guint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_applications")))

(define (GtkRecentInfo-get-description self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_description")))

(define (GtkRecentInfo-get-display-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_display_name")))

(define (GtkRecentInfo-get-gicon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_gicon")))

(define (GtkRecentInfo-get-groups self)
"  ARGS: 
     length  - exact integer of size guint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_groups")))

(define (GtkRecentInfo-get-icon self size)
"  ARGS: 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon" size)))

(define (GtkRecentInfo-get-mime-type self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_mime_type")))

(define (GtkRecentInfo-get-modified self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_modified")))

(define (GtkRecentInfo-get-private-hint? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_private_hint")))

(define (GtkRecentInfo-get-short-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_short_name")))

(define (GtkRecentInfo-get-uri self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_uri")))

(define (GtkRecentInfo-get-uri-display self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_uri_display")))

(define (GtkRecentInfo-get-visited self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_visited")))

(define (GtkRecentInfo-has-application? self app-name)
"  ARGS: 
     app-name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_application" app-name)))

(define (GtkRecentInfo-has-group? self group-name)
"  ARGS: 
     group-name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_group" group-name)))

(define (GtkRecentInfo-is-local? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_local")))

(define (GtkRecentInfo-last-application self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "last_application")))

(define (GtkRecentInfo-match? self info-b)
"  ARGS: 
     info-b  - struct RecentInfo
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "match" info-b)))

(define (GtkRecentInfo-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkRecentInfo-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define <GtkRecentManager>
  (gi-lookup-type "Gtk-RecentManager"))

(define (GtkRecentManager-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentManager-new"))

(define (GtkRecentManager-get-default)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RecentManager-get_default"))

(define (GtkRecentManager-add-full? self uri recent-data)
"  ARGS: 
     uri  - string, 
     recent-data  - struct RecentData
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "add_full" uri recent-data)))

(define (GtkRecentManager-add-item? self uri)
"  ARGS: 
     uri  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "add_item" uri)))

(define (GtkRecentManager-get-items self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_items")))

(define (GtkRecentManager-has-item? self uri)
"  ARGS: 
     uri  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_item" uri)))

(define (GtkRecentManager-lookup-item self uri)
"  ARGS: 
     uri  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_item" uri)))

(define (GtkRecentManager-move-item? self uri new-uri)
"  ARGS: 
     uri  - string, 
     new-uri  - #f for NULL or string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "move_item" uri new-uri)))

(define (GtkRecentManager-purge-items self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "purge_items")))

(define (GtkRecentManager-remove-item? self uri)
"  ARGS: 
     uri  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "remove_item" uri)))

(define RECENT_MANAGER_ERROR_NOT_FOUND
  (gi-enum-value "Gtk-RecentManagerError" "not_found"))

(define RECENT_MANAGER_ERROR_INVALID_URI
  (gi-enum-value "Gtk-RecentManagerError" "invalid_uri"))

(define RECENT_MANAGER_ERROR_INVALID_ENCODING
  (gi-enum-value "Gtk-RecentManagerError" "invalid_encoding"))

(define RECENT_MANAGER_ERROR_NOT_REGISTERED
  (gi-enum-value "Gtk-RecentManagerError" "not_registered"))

(define RECENT_MANAGER_ERROR_READ
  (gi-enum-value "Gtk-RecentManagerError" "read"))

(define RECENT_MANAGER_ERROR_WRITE
  (gi-enum-value "Gtk-RecentManagerError" "write"))

(define RECENT_MANAGER_ERROR_UNKNOWN
  (gi-enum-value "Gtk-RecentManagerError" "unknown"))

;; CALLBACK
(define recent-sort-func
  (gi-lookup-callback-info "Gtk-RecentSortFunc"))
;; ARGS: 
;;   a  - struct RecentInfo, 
;;   b  - struct RecentInfo, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gint32
(define RECENT_SORT_TYPE_NONE
  (gi-enum-value "Gtk-RecentSortType" "none"))

(define RECENT_SORT_TYPE_MRU
  (gi-enum-value "Gtk-RecentSortType" "mru"))

(define RECENT_SORT_TYPE_LRU
  (gi-enum-value "Gtk-RecentSortType" "lru"))

(define RECENT_SORT_TYPE_CUSTOM
  (gi-enum-value "Gtk-RecentSortType" "custom"))

(define EVEN
  (gi-flag-value "Gtk-RegionFlags" "even"))

(define ODD
  (gi-flag-value "Gtk-RegionFlags" "odd"))

(define FIRST
  (gi-flag-value "Gtk-RegionFlags" "first"))

(define LAST
  (gi-flag-value "Gtk-RegionFlags" "last"))

(define ONLY
  (gi-flag-value "Gtk-RegionFlags" "only"))

(define SORTED
  (gi-flag-value "Gtk-RegionFlags" "sorted"))

(define RELIEF_STYLE_NORMAL
  (gi-enum-value "Gtk-ReliefStyle" "normal"))

(define RELIEF_STYLE_HALF
  (gi-enum-value "Gtk-ReliefStyle" "half"))

(define RELIEF_STYLE_NONE
  (gi-enum-value "Gtk-ReliefStyle" "none"))

(define <GtkRendererCellAccessible>
  (gi-lookup-type "Gtk-RendererCellAccessible"))

(define (GtkRendererCellAccessible-new renderer)
"  ARGS: 
     renderer  - object CellRenderer
   RETURN: interface*
"
  (gi-function-invoke "Gtk-RendererCellAccessible-new" renderer))

(define <GtkRequisition>
  (gi-lookup-type "Gtk-Requisition"))

(define (GtkRequisition-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Requisition-new"))

(define (GtkRequisition-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkRequisition-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define RESIZE_MODE_PARENT
  (gi-enum-value "Gtk-ResizeMode" "parent"))

(define RESIZE_MODE_QUEUE
  (gi-enum-value "Gtk-ResizeMode" "queue"))

(define RESIZE_MODE_IMMEDIATE
  (gi-enum-value "Gtk-ResizeMode" "immediate"))

(define RESPONSE_TYPE_NONE
  (gi-enum-value "Gtk-ResponseType" "none"))

(define RESPONSE_TYPE_REJECT
  (gi-enum-value "Gtk-ResponseType" "reject"))

(define RESPONSE_TYPE_ACCEPT
  (gi-enum-value "Gtk-ResponseType" "accept"))

(define RESPONSE_TYPE_DELETE_EVENT
  (gi-enum-value "Gtk-ResponseType" "delete_event"))

(define RESPONSE_TYPE_OK
  (gi-enum-value "Gtk-ResponseType" "ok"))

(define RESPONSE_TYPE_CANCEL
  (gi-enum-value "Gtk-ResponseType" "cancel"))

(define RESPONSE_TYPE_CLOSE
  (gi-enum-value "Gtk-ResponseType" "close"))

(define RESPONSE_TYPE_YES
  (gi-enum-value "Gtk-ResponseType" "yes"))

(define RESPONSE_TYPE_NO
  (gi-enum-value "Gtk-ResponseType" "no"))

(define RESPONSE_TYPE_APPLY
  (gi-enum-value "Gtk-ResponseType" "apply"))

(define RESPONSE_TYPE_HELP
  (gi-enum-value "Gtk-ResponseType" "help"))

(define <GtkRevealer>
  (gi-lookup-type "Gtk-Revealer"))

(define (GtkRevealer-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Revealer-new"))

(define (GtkRevealer-get-child-revealed? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_child_revealed")))

(define (GtkRevealer-get-reveal-child? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reveal_child")))

(define (GtkRevealer-get-transition-duration self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_transition_duration")))

(define (GtkRevealer-get-transition-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_transition_type")))

(define (GtkRevealer-set-reveal-child self reveal-child)
"  ARGS: 
     reveal-child  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reveal_child" reveal-child)))

(define (GtkRevealer-set-transition-duration self duration)
"  ARGS: 
     duration  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transition_duration" duration)))

(define (GtkRevealer-set-transition-type self transition)
"  ARGS: 
     transition  - exact integer of enum type RevealerTransitionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transition_type" transition)))

(define REVEALER_TRANSITION_TYPE_NONE
  (gi-enum-value "Gtk-RevealerTransitionType" "none"))

(define REVEALER_TRANSITION_TYPE_CROSSFADE
  (gi-enum-value "Gtk-RevealerTransitionType" "crossfade"))

(define REVEALER_TRANSITION_TYPE_SLIDE_RIGHT
  (gi-enum-value "Gtk-RevealerTransitionType" "slide_right"))

(define REVEALER_TRANSITION_TYPE_SLIDE_LEFT
  (gi-enum-value "Gtk-RevealerTransitionType" "slide_left"))

(define REVEALER_TRANSITION_TYPE_SLIDE_UP
  (gi-enum-value "Gtk-RevealerTransitionType" "slide_up"))

(define REVEALER_TRANSITION_TYPE_SLIDE_DOWN
  (gi-enum-value "Gtk-RevealerTransitionType" "slide_down"))

(define GTK_STYLE_CLASS_ACCELERATOR
  (gi-constant-value "Gtk-STYLE_CLASS_ACCELERATOR"))

(define GTK_STYLE_CLASS_ARROW
  (gi-constant-value "Gtk-STYLE_CLASS_ARROW"))

(define GTK_STYLE_CLASS_BACKGROUND
  (gi-constant-value "Gtk-STYLE_CLASS_BACKGROUND"))

(define GTK_STYLE_CLASS_BOTTOM
  (gi-constant-value "Gtk-STYLE_CLASS_BOTTOM"))

(define GTK_STYLE_CLASS_BUTTON
  (gi-constant-value "Gtk-STYLE_CLASS_BUTTON"))

(define GTK_STYLE_CLASS_CALENDAR
  (gi-constant-value "Gtk-STYLE_CLASS_CALENDAR"))

(define GTK_STYLE_CLASS_CELL
  (gi-constant-value "Gtk-STYLE_CLASS_CELL"))

(define GTK_STYLE_CLASS_CHECK
  (gi-constant-value "Gtk-STYLE_CLASS_CHECK"))

(define GTK_STYLE_CLASS_COMBOBOX_ENTRY
  (gi-constant-value "Gtk-STYLE_CLASS_COMBOBOX_ENTRY"))

(define GTK_STYLE_CLASS_CONTEXT_MENU
  (gi-constant-value "Gtk-STYLE_CLASS_CONTEXT_MENU"))

(define GTK_STYLE_CLASS_CSD
  (gi-constant-value "Gtk-STYLE_CLASS_CSD"))

(define GTK_STYLE_CLASS_CURSOR_HANDLE
  (gi-constant-value "Gtk-STYLE_CLASS_CURSOR_HANDLE"))

(define GTK_STYLE_CLASS_DEFAULT
  (gi-constant-value "Gtk-STYLE_CLASS_DEFAULT"))

(define GTK_STYLE_CLASS_DESTRUCTIVE_ACTION
  (gi-constant-value "Gtk-STYLE_CLASS_DESTRUCTIVE_ACTION"))

(define GTK_STYLE_CLASS_DIM_LABEL
  (gi-constant-value "Gtk-STYLE_CLASS_DIM_LABEL"))

(define GTK_STYLE_CLASS_DND
  (gi-constant-value "Gtk-STYLE_CLASS_DND"))

(define GTK_STYLE_CLASS_DOCK
  (gi-constant-value "Gtk-STYLE_CLASS_DOCK"))

(define GTK_STYLE_CLASS_ENTRY
  (gi-constant-value "Gtk-STYLE_CLASS_ENTRY"))

(define GTK_STYLE_CLASS_ERROR
  (gi-constant-value "Gtk-STYLE_CLASS_ERROR"))

(define GTK_STYLE_CLASS_EXPANDER
  (gi-constant-value "Gtk-STYLE_CLASS_EXPANDER"))

(define GTK_STYLE_CLASS_FLAT
  (gi-constant-value "Gtk-STYLE_CLASS_FLAT"))

(define GTK_STYLE_CLASS_FRAME
  (gi-constant-value "Gtk-STYLE_CLASS_FRAME"))

(define GTK_STYLE_CLASS_GRIP
  (gi-constant-value "Gtk-STYLE_CLASS_GRIP"))

(define GTK_STYLE_CLASS_HEADER
  (gi-constant-value "Gtk-STYLE_CLASS_HEADER"))

(define GTK_STYLE_CLASS_HIGHLIGHT
  (gi-constant-value "Gtk-STYLE_CLASS_HIGHLIGHT"))

(define GTK_STYLE_CLASS_HORIZONTAL
  (gi-constant-value "Gtk-STYLE_CLASS_HORIZONTAL"))

(define GTK_STYLE_CLASS_IMAGE
  (gi-constant-value "Gtk-STYLE_CLASS_IMAGE"))

(define GTK_STYLE_CLASS_INFO
  (gi-constant-value "Gtk-STYLE_CLASS_INFO"))

(define GTK_STYLE_CLASS_INLINE_TOOLBAR
  (gi-constant-value "Gtk-STYLE_CLASS_INLINE_TOOLBAR"))

(define GTK_STYLE_CLASS_INSERTION_CURSOR
  (gi-constant-value "Gtk-STYLE_CLASS_INSERTION_CURSOR"))

(define GTK_STYLE_CLASS_LABEL
  (gi-constant-value "Gtk-STYLE_CLASS_LABEL"))

(define GTK_STYLE_CLASS_LEFT
  (gi-constant-value "Gtk-STYLE_CLASS_LEFT"))

(define GTK_STYLE_CLASS_LEVEL_BAR
  (gi-constant-value "Gtk-STYLE_CLASS_LEVEL_BAR"))

(define GTK_STYLE_CLASS_LINKED
  (gi-constant-value "Gtk-STYLE_CLASS_LINKED"))

(define GTK_STYLE_CLASS_LIST
  (gi-constant-value "Gtk-STYLE_CLASS_LIST"))

(define GTK_STYLE_CLASS_LIST_ROW
  (gi-constant-value "Gtk-STYLE_CLASS_LIST_ROW"))

(define GTK_STYLE_CLASS_MARK
  (gi-constant-value "Gtk-STYLE_CLASS_MARK"))

(define GTK_STYLE_CLASS_MENU
  (gi-constant-value "Gtk-STYLE_CLASS_MENU"))

(define GTK_STYLE_CLASS_MENUBAR
  (gi-constant-value "Gtk-STYLE_CLASS_MENUBAR"))

(define GTK_STYLE_CLASS_MENUITEM
  (gi-constant-value "Gtk-STYLE_CLASS_MENUITEM"))

(define GTK_STYLE_CLASS_MESSAGE_DIALOG
  (gi-constant-value "Gtk-STYLE_CLASS_MESSAGE_DIALOG"))

(define GTK_STYLE_CLASS_MONOSPACE
  (gi-constant-value "Gtk-STYLE_CLASS_MONOSPACE"))

(define GTK_STYLE_CLASS_NEEDS_ATTENTION
  (gi-constant-value "Gtk-STYLE_CLASS_NEEDS_ATTENTION"))

(define GTK_STYLE_CLASS_NOTEBOOK
  (gi-constant-value "Gtk-STYLE_CLASS_NOTEBOOK"))

(define GTK_STYLE_CLASS_OSD
  (gi-constant-value "Gtk-STYLE_CLASS_OSD"))

(define GTK_STYLE_CLASS_OVERSHOOT
  (gi-constant-value "Gtk-STYLE_CLASS_OVERSHOOT"))

(define GTK_STYLE_CLASS_PANE_SEPARATOR
  (gi-constant-value "Gtk-STYLE_CLASS_PANE_SEPARATOR"))

(define GTK_STYLE_CLASS_PAPER
  (gi-constant-value "Gtk-STYLE_CLASS_PAPER"))

(define GTK_STYLE_CLASS_POPOVER
  (gi-constant-value "Gtk-STYLE_CLASS_POPOVER"))

(define GTK_STYLE_CLASS_POPUP
  (gi-constant-value "Gtk-STYLE_CLASS_POPUP"))

(define GTK_STYLE_CLASS_PRIMARY_TOOLBAR
  (gi-constant-value "Gtk-STYLE_CLASS_PRIMARY_TOOLBAR"))

(define GTK_STYLE_CLASS_PROGRESSBAR
  (gi-constant-value "Gtk-STYLE_CLASS_PROGRESSBAR"))

(define GTK_STYLE_CLASS_PULSE
  (gi-constant-value "Gtk-STYLE_CLASS_PULSE"))

(define GTK_STYLE_CLASS_QUESTION
  (gi-constant-value "Gtk-STYLE_CLASS_QUESTION"))

(define GTK_STYLE_CLASS_RADIO
  (gi-constant-value "Gtk-STYLE_CLASS_RADIO"))

(define GTK_STYLE_CLASS_RAISED
  (gi-constant-value "Gtk-STYLE_CLASS_RAISED"))

(define GTK_STYLE_CLASS_READ_ONLY
  (gi-constant-value "Gtk-STYLE_CLASS_READ_ONLY"))

(define GTK_STYLE_CLASS_RIGHT
  (gi-constant-value "Gtk-STYLE_CLASS_RIGHT"))

(define GTK_STYLE_CLASS_RUBBERBAND
  (gi-constant-value "Gtk-STYLE_CLASS_RUBBERBAND"))

(define GTK_STYLE_CLASS_SCALE
  (gi-constant-value "Gtk-STYLE_CLASS_SCALE"))

(define GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
  (gi-constant-value "Gtk-STYLE_CLASS_SCALE_HAS_MARKS_ABOVE"))

(define GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
  (gi-constant-value "Gtk-STYLE_CLASS_SCALE_HAS_MARKS_BELOW"))

(define GTK_STYLE_CLASS_SCROLLBAR
  (gi-constant-value "Gtk-STYLE_CLASS_SCROLLBAR"))

(define GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
  (gi-constant-value "Gtk-STYLE_CLASS_SCROLLBARS_JUNCTION"))

(define GTK_STYLE_CLASS_SEPARATOR
  (gi-constant-value "Gtk-STYLE_CLASS_SEPARATOR"))

(define GTK_STYLE_CLASS_SIDEBAR
  (gi-constant-value "Gtk-STYLE_CLASS_SIDEBAR"))

(define GTK_STYLE_CLASS_SLIDER
  (gi-constant-value "Gtk-STYLE_CLASS_SLIDER"))

(define GTK_STYLE_CLASS_SPINBUTTON
  (gi-constant-value "Gtk-STYLE_CLASS_SPINBUTTON"))

(define GTK_STYLE_CLASS_SPINNER
  (gi-constant-value "Gtk-STYLE_CLASS_SPINNER"))

(define GTK_STYLE_CLASS_STATUSBAR
  (gi-constant-value "Gtk-STYLE_CLASS_STATUSBAR"))

(define GTK_STYLE_CLASS_SUBTITLE
  (gi-constant-value "Gtk-STYLE_CLASS_SUBTITLE"))

(define GTK_STYLE_CLASS_SUGGESTED_ACTION
  (gi-constant-value "Gtk-STYLE_CLASS_SUGGESTED_ACTION"))

(define GTK_STYLE_CLASS_TITLE
  (gi-constant-value "Gtk-STYLE_CLASS_TITLE"))

(define GTK_STYLE_CLASS_TITLEBAR
  (gi-constant-value "Gtk-STYLE_CLASS_TITLEBAR"))

(define GTK_STYLE_CLASS_TOOLBAR
  (gi-constant-value "Gtk-STYLE_CLASS_TOOLBAR"))

(define GTK_STYLE_CLASS_TOOLTIP
  (gi-constant-value "Gtk-STYLE_CLASS_TOOLTIP"))

(define GTK_STYLE_CLASS_TOP
  (gi-constant-value "Gtk-STYLE_CLASS_TOP"))

(define GTK_STYLE_CLASS_TOUCH_SELECTION
  (gi-constant-value "Gtk-STYLE_CLASS_TOUCH_SELECTION"))

(define GTK_STYLE_CLASS_TROUGH
  (gi-constant-value "Gtk-STYLE_CLASS_TROUGH"))

(define GTK_STYLE_CLASS_UNDERSHOOT
  (gi-constant-value "Gtk-STYLE_CLASS_UNDERSHOOT"))

(define GTK_STYLE_CLASS_VERTICAL
  (gi-constant-value "Gtk-STYLE_CLASS_VERTICAL"))

(define GTK_STYLE_CLASS_VIEW
  (gi-constant-value "Gtk-STYLE_CLASS_VIEW"))

(define GTK_STYLE_CLASS_WARNING
  (gi-constant-value "Gtk-STYLE_CLASS_WARNING"))

(define GTK_STYLE_CLASS_WIDE
  (gi-constant-value "Gtk-STYLE_CLASS_WIDE"))

(define GTK_STYLE_PROPERTY_BACKGROUND_COLOR
  (gi-constant-value "Gtk-STYLE_PROPERTY_BACKGROUND_COLOR"))

(define GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
  (gi-constant-value "Gtk-STYLE_PROPERTY_BACKGROUND_IMAGE"))

(define GTK_STYLE_PROPERTY_BORDER_COLOR
  (gi-constant-value "Gtk-STYLE_PROPERTY_BORDER_COLOR"))

(define GTK_STYLE_PROPERTY_BORDER_RADIUS
  (gi-constant-value "Gtk-STYLE_PROPERTY_BORDER_RADIUS"))

(define GTK_STYLE_PROPERTY_BORDER_STYLE
  (gi-constant-value "Gtk-STYLE_PROPERTY_BORDER_STYLE"))

(define GTK_STYLE_PROPERTY_BORDER_WIDTH
  (gi-constant-value "Gtk-STYLE_PROPERTY_BORDER_WIDTH"))

(define GTK_STYLE_PROPERTY_COLOR
  (gi-constant-value "Gtk-STYLE_PROPERTY_COLOR"))

(define GTK_STYLE_PROPERTY_FONT
  (gi-constant-value "Gtk-STYLE_PROPERTY_FONT"))

(define GTK_STYLE_PROPERTY_MARGIN
  (gi-constant-value "Gtk-STYLE_PROPERTY_MARGIN"))

(define GTK_STYLE_PROPERTY_PADDING
  (gi-constant-value "Gtk-STYLE_PROPERTY_PADDING"))

(define GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
  (gi-constant-value "Gtk-STYLE_PROVIDER_PRIORITY_APPLICATION"))

(define GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
  (gi-constant-value "Gtk-STYLE_PROVIDER_PRIORITY_FALLBACK"))

(define GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
  (gi-constant-value "Gtk-STYLE_PROVIDER_PRIORITY_SETTINGS"))

(define GTK_STYLE_PROVIDER_PRIORITY_THEME
  (gi-constant-value "Gtk-STYLE_PROVIDER_PRIORITY_THEME"))

(define GTK_STYLE_PROVIDER_PRIORITY_USER
  (gi-constant-value "Gtk-STYLE_PROVIDER_PRIORITY_USER"))

(define <GtkScale>
  (gi-lookup-type "Gtk-Scale"))

(define (GtkScale-new orientation adjustment)
"  ARGS: 
     orientation  - exact integer of enum type Orientation, 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Scale-new" orientation adjustment))

(define (GtkScale-new-with-range orientation min max step)
"  ARGS: 
     orientation  - exact integer of enum type Orientation, 
     min  - real number of size gdouble, 
     max  - real number of size gdouble, 
     step  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Scale-new_with_range" orientation min max step))

(define (GtkScale-add-mark self value position markup)
"  ARGS: 
     value  - real number of size gdouble, 
     position  - exact integer of enum type PositionType, 
     markup  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mark" value position markup)))

(define (GtkScale-clear-marks self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear_marks")))

(define (GtkScale-get-digits self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_digits")))

(define (GtkScale-get-draw-value? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_draw_value")))

(define (GtkScale-get-has-origin? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_origin")))

(define (GtkScale-get-layout self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_layout")))

(define (GtkScale-get-layout-offsets self)
"  ARGS: 
     x  - exact integer of size gint32[OUT], 
     y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_layout_offsets")))

(define (GtkScale-get-value-pos self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_value_pos")))

(define (GtkScale-set-digits self digits)
"  ARGS: 
     digits  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_digits" digits)))

(define (GtkScale-set-draw-value self draw-value)
"  ARGS: 
     draw-value  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_draw_value" draw-value)))

(define (GtkScale-set-has-origin self has-origin)
"  ARGS: 
     has-origin  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_origin" has-origin)))

(define (GtkScale-set-value-pos self pos)
"  ARGS: 
     pos  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value_pos" pos)))

(define <GtkScaleAccessible>
  (gi-lookup-type "Gtk-ScaleAccessible"))

(define <GtkScaleButton>
  (gi-lookup-type "Gtk-ScaleButton"))

(define (GtkScaleButton-new size min max step icons)
"  ARGS: 
     size  - exact integer of size gint32, 
     min  - real number of size gdouble, 
     max  - real number of size gdouble, 
     step  - real number of size gdouble, 
     icons  - #f for NULL or Unhandled argument type tag 15
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ScaleButton-new" size min max step icons))

(define (GtkScaleButton-get-adjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_adjustment")))

(define (GtkScaleButton-get-minus-button self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_minus_button")))

(define (GtkScaleButton-get-plus-button self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_plus_button")))

(define (GtkScaleButton-get-popup self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_popup")))

(define (GtkScaleButton-get-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_value")))

(define (GtkScaleButton-set-adjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_adjustment" adjustment)))

(define (GtkScaleButton-set-icons self icons)
"  ARGS: 
     icons  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icons" icons)))

(define (GtkScaleButton-set-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" value)))

(define <GtkScaleButtonAccessible>
  (gi-lookup-type "Gtk-ScaleButtonAccessible"))

(define SCROLL_STEP_STEPS
  (gi-enum-value "Gtk-ScrollStep" "steps"))

(define SCROLL_STEP_PAGES
  (gi-enum-value "Gtk-ScrollStep" "pages"))

(define SCROLL_STEP_ENDS
  (gi-enum-value "Gtk-ScrollStep" "ends"))

(define SCROLL_STEP_HORIZONTAL_STEPS
  (gi-enum-value "Gtk-ScrollStep" "horizontal_steps"))

(define SCROLL_STEP_HORIZONTAL_PAGES
  (gi-enum-value "Gtk-ScrollStep" "horizontal_pages"))

(define SCROLL_STEP_HORIZONTAL_ENDS
  (gi-enum-value "Gtk-ScrollStep" "horizontal_ends"))

(define SCROLL_TYPE_NONE
  (gi-enum-value "Gtk-ScrollType" "none"))

(define SCROLL_TYPE_JUMP
  (gi-enum-value "Gtk-ScrollType" "jump"))

(define SCROLL_TYPE_STEP_BACKWARD
  (gi-enum-value "Gtk-ScrollType" "step_backward"))

(define SCROLL_TYPE_STEP_FORWARD
  (gi-enum-value "Gtk-ScrollType" "step_forward"))

(define SCROLL_TYPE_PAGE_BACKWARD
  (gi-enum-value "Gtk-ScrollType" "page_backward"))

(define SCROLL_TYPE_PAGE_FORWARD
  (gi-enum-value "Gtk-ScrollType" "page_forward"))

(define SCROLL_TYPE_STEP_UP
  (gi-enum-value "Gtk-ScrollType" "step_up"))

(define SCROLL_TYPE_STEP_DOWN
  (gi-enum-value "Gtk-ScrollType" "step_down"))

(define SCROLL_TYPE_PAGE_UP
  (gi-enum-value "Gtk-ScrollType" "page_up"))

(define SCROLL_TYPE_PAGE_DOWN
  (gi-enum-value "Gtk-ScrollType" "page_down"))

(define SCROLL_TYPE_STEP_LEFT
  (gi-enum-value "Gtk-ScrollType" "step_left"))

(define SCROLL_TYPE_STEP_RIGHT
  (gi-enum-value "Gtk-ScrollType" "step_right"))

(define SCROLL_TYPE_PAGE_LEFT
  (gi-enum-value "Gtk-ScrollType" "page_left"))

(define SCROLL_TYPE_PAGE_RIGHT
  (gi-enum-value "Gtk-ScrollType" "page_right"))

(define SCROLL_TYPE_START
  (gi-enum-value "Gtk-ScrollType" "start"))

(define SCROLL_TYPE_END
  (gi-enum-value "Gtk-ScrollType" "end"))

(define SCROLLABLE_POLICY_MINIMUM
  (gi-enum-value "Gtk-ScrollablePolicy" "minimum"))

(define SCROLLABLE_POLICY_NATURAL
  (gi-enum-value "Gtk-ScrollablePolicy" "natural"))

(define <GtkScrollbar>
  (gi-lookup-type "Gtk-Scrollbar"))

(define (GtkScrollbar-new orientation adjustment)
"  ARGS: 
     orientation  - exact integer of enum type Orientation, 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Scrollbar-new" orientation adjustment))

(define <GtkScrolledWindow>
  (gi-lookup-type "Gtk-ScrolledWindow"))

(define (GtkScrolledWindow-new hadjustment vadjustment)
"  ARGS: 
     hadjustment  - object Adjustment, 
     vadjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ScrolledWindow-new" hadjustment vadjustment))

(define (GtkScrolledWindow-add-with-viewport self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_with_viewport" child)))

(define (GtkScrolledWindow-get-capture-button-press? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_capture_button_press")))

(define (GtkScrolledWindow-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkScrolledWindow-get-hscrollbar self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hscrollbar")))

(define (GtkScrolledWindow-get-kinetic-scrolling? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_kinetic_scrolling")))

(define (GtkScrolledWindow-get-max-content-height self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_content_height")))

(define (GtkScrolledWindow-get-max-content-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_content_width")))

(define (GtkScrolledWindow-get-min-content-height self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_min_content_height")))

(define (GtkScrolledWindow-get-min-content-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_min_content_width")))

(define (GtkScrolledWindow-get-overlay-scrolling? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_overlay_scrolling")))

(define (GtkScrolledWindow-get-placement self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_placement")))

(define (GtkScrolledWindow-get-policy self)
"  ARGS: 
     hscrollbar-policy  - exact integer of enum type PolicyType[OUT], 
     vscrollbar-policy  - exact integer of enum type PolicyType[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_policy")))

(define (GtkScrolledWindow-get-propagate-natural-height? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_propagate_natural_height")))

(define (GtkScrolledWindow-get-propagate-natural-width? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_propagate_natural_width")))

(define (GtkScrolledWindow-get-shadow-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_shadow_type")))

(define (GtkScrolledWindow-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkScrolledWindow-get-vscrollbar self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vscrollbar")))

(define (GtkScrolledWindow-set-capture-button-press self capture-button-press)
"  ARGS: 
     capture-button-press  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_capture_button_press" capture-button-press)))

(define (GtkScrolledWindow-set-hadjustment self hadjustment)
"  ARGS: 
     hadjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hadjustment" hadjustment)))

(define (GtkScrolledWindow-set-kinetic-scrolling self kinetic-scrolling)
"  ARGS: 
     kinetic-scrolling  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_kinetic_scrolling" kinetic-scrolling)))

(define (GtkScrolledWindow-set-max-content-height self height)
"  ARGS: 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_content_height" height)))

(define (GtkScrolledWindow-set-max-content-width self width)
"  ARGS: 
     width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_content_width" width)))

(define (GtkScrolledWindow-set-min-content-height self height)
"  ARGS: 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_content_height" height)))

(define (GtkScrolledWindow-set-min-content-width self width)
"  ARGS: 
     width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_content_width" width)))

(define (GtkScrolledWindow-set-overlay-scrolling self overlay-scrolling)
"  ARGS: 
     overlay-scrolling  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_overlay_scrolling" overlay-scrolling)))

(define (GtkScrolledWindow-set-placement self window-placement)
"  ARGS: 
     window-placement  - exact integer of enum type CornerType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_placement" window-placement)))

(define (GtkScrolledWindow-set-policy self hscrollbar-policy vscrollbar-policy)
"  ARGS: 
     hscrollbar-policy  - exact integer of enum type PolicyType, 
     vscrollbar-policy  - exact integer of enum type PolicyType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_policy" hscrollbar-policy vscrollbar-policy)))

(define (GtkScrolledWindow-set-propagate-natural-height self propagate)
"  ARGS: 
     propagate  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_propagate_natural_height" propagate)))

(define (GtkScrolledWindow-set-propagate-natural-width self propagate)
"  ARGS: 
     propagate  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_propagate_natural_width" propagate)))

(define (GtkScrolledWindow-set-shadow-type self type)
"  ARGS: 
     type  - exact integer of enum type ShadowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_shadow_type" type)))

(define (GtkScrolledWindow-set-vadjustment self vadjustment)
"  ARGS: 
     vadjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vadjustment" vadjustment)))

(define (GtkScrolledWindow-unset-placement self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_placement")))

(define <GtkScrolledWindowAccessible>
  (gi-lookup-type "Gtk-ScrolledWindowAccessible"))

(define <GtkSearchBar>
  (gi-lookup-type "Gtk-SearchBar"))

(define (GtkSearchBar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SearchBar-new"))

(define (GtkSearchBar-connect-entry self entry)
"  ARGS: 
     entry  - object Entry
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "connect_entry" entry)))

(define (GtkSearchBar-get-search-mode? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_search_mode")))

(define (GtkSearchBar-get-show-close-button? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_close_button")))

(define (GtkSearchBar-handle-event? self event)
"  ARGS: 
     event  - union Event
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "handle_event" event)))

(define (GtkSearchBar-set-search-mode self search-mode)
"  ARGS: 
     search-mode  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_mode" search-mode)))

(define (GtkSearchBar-set-show-close-button self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_close_button" visible)))

(define <GtkSearchEntry>
  (gi-lookup-type "Gtk-SearchEntry"))

(define (GtkSearchEntry-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SearchEntry-new"))

(define (GtkSearchEntry-handle-event? self event)
"  ARGS: 
     event  - union Event
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "handle_event" event)))

(define <GtkSelectionData>
  (gi-lookup-type "Gtk-SelectionData"))

(define (GtkSelectionData-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkSelectionData-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkSelectionData-get-data-type self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_data_type")))

(define (GtkSelectionData-get-data self)
"  ARGS: 
     length  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_data")))

(define (GtkSelectionData-get-display self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_display")))

(define (GtkSelectionData-get-format self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_format")))

(define (GtkSelectionData-get-length self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_length")))

(define (GtkSelectionData-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkSelectionData-get-selection self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_selection")))

(define (GtkSelectionData-get-target self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_target")))

(define (GtkSelectionData-get-targets? self)
"  ARGS: 
     targets  - Unhandled argument type tag 15[OUT], 
     n-atoms  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_targets")))

(define (GtkSelectionData-get-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text")))

(define (GtkSelectionData-get-uris self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_uris")))

(define (GtkSelectionData-set self type format data length)
"  ARGS: 
     type  - struct Atom, 
     format  - exact integer of size gint32, 
     data  - Unhandled argument type tag 15, 
     length  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" type format data length)))

(define (GtkSelectionData-set-pixbuf? self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_pixbuf" pixbuf)))

(define (GtkSelectionData-set-text? self str len)
"  ARGS: 
     str  - string, 
     len  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_text" str len)))

(define (GtkSelectionData-set-uris? self uris)
"  ARGS: 
     uris  - Unhandled argument type tag 15
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_uris" uris)))

(define (GtkSelectionData-targets-include-image? self writable)
"  ARGS: 
     writable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "targets_include_image" writable)))

(define (GtkSelectionData-targets-include-rich-text? self buffer)
"  ARGS: 
     buffer  - object TextBuffer
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "targets_include_rich_text" buffer)))

(define (GtkSelectionData-targets-include-text? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "targets_include_text")))

(define (GtkSelectionData-targets-include-uri? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "targets_include_uri")))

(define SELECTION_MODE_NONE
  (gi-enum-value "Gtk-SelectionMode" "none"))

(define SELECTION_MODE_SINGLE
  (gi-enum-value "Gtk-SelectionMode" "single"))

(define SELECTION_MODE_BROWSE
  (gi-enum-value "Gtk-SelectionMode" "browse"))

(define SELECTION_MODE_MULTIPLE
  (gi-enum-value "Gtk-SelectionMode" "multiple"))

(define SENSITIVITY_TYPE_AUTO
  (gi-enum-value "Gtk-SensitivityType" "auto"))

(define SENSITIVITY_TYPE_ON
  (gi-enum-value "Gtk-SensitivityType" "on"))

(define SENSITIVITY_TYPE_OFF
  (gi-enum-value "Gtk-SensitivityType" "off"))

(define <GtkSeparator>
  (gi-lookup-type "Gtk-Separator"))

(define (GtkSeparator-new orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Separator-new" orientation))

(define <GtkSeparatorMenuItem>
  (gi-lookup-type "Gtk-SeparatorMenuItem"))

(define (GtkSeparatorMenuItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SeparatorMenuItem-new"))

(define <GtkSeparatorToolItem>
  (gi-lookup-type "Gtk-SeparatorToolItem"))

(define (GtkSeparatorToolItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SeparatorToolItem-new"))

(define (GtkSeparatorToolItem-get-draw? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_draw")))

(define (GtkSeparatorToolItem-set-draw self draw)
"  ARGS: 
     draw  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_draw" draw)))

(define <GtkSettings>
  (gi-lookup-type "Gtk-Settings"))

(define (GtkSettings-get-default)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Settings-get_default"))

(define (GtkSettings-get-for-screen screen)
"  ARGS: 
     screen  - object Screen
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Settings-get_for_screen" screen))

(define (GtkSettings-install-property pspec)
"  ARGS: 
     pspec  - object ParamSpec
   RETURN: void
"
  (gi-function-invoke "Gtk-Settings-install_property" pspec))

(define (GtkSettings-install-property-parser pspec parser)
"  ARGS: 
     pspec  - object ParamSpec, 
     parser  - procedure of type RcPropertyParser
   RETURN: void
"
  (gi-function-invoke "Gtk-Settings-install_property_parser" pspec parser))

(define (GtkSettings-reset-property self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_property" name)))

(define (GtkSettings-set-double-property self name v-double origin)
"  ARGS: 
     name  - string, 
     v-double  - real number of size gdouble, 
     origin  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_double_property" name v-double origin)))

(define (GtkSettings-set-long-property self name v-long origin)
"  ARGS: 
     name  - string, 
     v-long  - exact integer of size gint32, 
     origin  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_long_property" name v-long origin)))

(define (GtkSettings-set-property-value self name svalue)
"  ARGS: 
     name  - string, 
     svalue  - struct SettingsValue
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_property_value" name svalue)))

(define (GtkSettings-set-string-property self name v-string origin)
"  ARGS: 
     name  - string, 
     v-string  - string, 
     origin  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_string_property" name v-string origin)))

(define SHADOW_TYPE_NONE
  (gi-enum-value "Gtk-ShadowType" "none"))

(define SHADOW_TYPE_IN
  (gi-enum-value "Gtk-ShadowType" "in"))

(define SHADOW_TYPE_OUT
  (gi-enum-value "Gtk-ShadowType" "out"))

(define SHADOW_TYPE_ETCHED_IN
  (gi-enum-value "Gtk-ShadowType" "etched_in"))

(define SHADOW_TYPE_ETCHED_OUT
  (gi-enum-value "Gtk-ShadowType" "etched_out"))

(define <GtkShortcutLabel>
  (gi-lookup-type "Gtk-ShortcutLabel"))

(define (GtkShortcutLabel-new accelerator)
"  ARGS: 
     accelerator  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ShortcutLabel-new" accelerator))

(define (GtkShortcutLabel-get-accelerator self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_accelerator")))

(define (GtkShortcutLabel-get-disabled-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_disabled_text")))

(define (GtkShortcutLabel-set-accelerator self accelerator)
"  ARGS: 
     accelerator  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accelerator" accelerator)))

(define (GtkShortcutLabel-set-disabled-text self disabled-text)
"  ARGS: 
     disabled-text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_disabled_text" disabled-text)))

(define SHORTCUT_TYPE_ACCELERATOR
  (gi-enum-value "Gtk-ShortcutType" "accelerator"))

(define SHORTCUT_TYPE_GESTURE_PINCH
  (gi-enum-value "Gtk-ShortcutType" "gesture_pinch"))

(define SHORTCUT_TYPE_GESTURE_STRETCH
  (gi-enum-value "Gtk-ShortcutType" "gesture_stretch"))

(define SHORTCUT_TYPE_GESTURE_ROTATE_CLOCKWISE
  (gi-enum-value "Gtk-ShortcutType" "gesture_rotate_clockwise"))

(define SHORTCUT_TYPE_GESTURE_ROTATE_COUNTERCLOCKWISE
  (gi-enum-value "Gtk-ShortcutType" "gesture_rotate_counterclockwise"))

(define SHORTCUT_TYPE_GESTURE_TWO_FINGER_SWIPE_LEFT
  (gi-enum-value "Gtk-ShortcutType" "gesture_two_finger_swipe_left"))

(define SHORTCUT_TYPE_GESTURE_TWO_FINGER_SWIPE_RIGHT
  (gi-enum-value "Gtk-ShortcutType" "gesture_two_finger_swipe_right"))

(define SHORTCUT_TYPE_GESTURE
  (gi-enum-value "Gtk-ShortcutType" "gesture"))

(define <GtkShortcutsGroup>
  (gi-lookup-type "Gtk-ShortcutsGroup"))

(define <GtkShortcutsSection>
  (gi-lookup-type "Gtk-ShortcutsSection"))

(define <GtkShortcutsShortcut>
  (gi-lookup-type "Gtk-ShortcutsShortcut"))

(define <GtkShortcutsWindow>
  (gi-lookup-type "Gtk-ShortcutsWindow"))

(define <GtkSizeGroup>
  (gi-lookup-type "Gtk-SizeGroup"))

(define (GtkSizeGroup-new mode)
"  ARGS: 
     mode  - exact integer of enum type SizeGroupMode
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SizeGroup-new" mode))

(define (GtkSizeGroup-add-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_widget" widget)))

(define (GtkSizeGroup-get-ignore-hidden? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_ignore_hidden")))

(define (GtkSizeGroup-get-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_mode")))

(define (GtkSizeGroup-get-widgets self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_widgets")))

(define (GtkSizeGroup-remove-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_widget" widget)))

(define (GtkSizeGroup-set-ignore-hidden self ignore-hidden)
"  ARGS: 
     ignore-hidden  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_ignore_hidden" ignore-hidden)))

(define (GtkSizeGroup-set-mode self mode)
"  ARGS: 
     mode  - exact integer of enum type SizeGroupMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mode" mode)))

(define SIZE_GROUP_MODE_NONE
  (gi-enum-value "Gtk-SizeGroupMode" "none"))

(define SIZE_GROUP_MODE_HORIZONTAL
  (gi-enum-value "Gtk-SizeGroupMode" "horizontal"))

(define SIZE_GROUP_MODE_VERTICAL
  (gi-enum-value "Gtk-SizeGroupMode" "vertical"))

(define SIZE_GROUP_MODE_BOTH
  (gi-enum-value "Gtk-SizeGroupMode" "both"))

(define SIZE_REQUEST_MODE_HEIGHT_FOR_WIDTH
  (gi-enum-value "Gtk-SizeRequestMode" "height_for_width"))

(define SIZE_REQUEST_MODE_WIDTH_FOR_HEIGHT
  (gi-enum-value "Gtk-SizeRequestMode" "width_for_height"))

(define SIZE_REQUEST_MODE_CONSTANT_SIZE
  (gi-enum-value "Gtk-SizeRequestMode" "constant_size"))

(define SORT_TYPE_ASCENDING
  (gi-enum-value "Gtk-SortType" "ascending"))

(define SORT_TYPE_DESCENDING
  (gi-enum-value "Gtk-SortType" "descending"))

(define <GtkSpinButton>
  (gi-lookup-type "Gtk-SpinButton"))

(define (GtkSpinButton-new adjustment climb-rate digits)
"  ARGS: 
     adjustment  - object Adjustment, 
     climb-rate  - real number of size gdouble, 
     digits  - exact integer of size guint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SpinButton-new" adjustment climb-rate digits))

(define (GtkSpinButton-new-with-range min max step)
"  ARGS: 
     min  - real number of size gdouble, 
     max  - real number of size gdouble, 
     step  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SpinButton-new_with_range" min max step))

(define (GtkSpinButton-configure self adjustment climb-rate digits)
"  ARGS: 
     adjustment  - object Adjustment, 
     climb-rate  - real number of size gdouble, 
     digits  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "configure" adjustment climb-rate digits)))

(define (GtkSpinButton-get-adjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_adjustment")))

(define (GtkSpinButton-get-digits self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_digits")))

(define (GtkSpinButton-get-increments self)
"  ARGS: 
     step  - real number of size gdouble[OUT], 
     page  - real number of size gdouble[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_increments")))

(define (GtkSpinButton-get-numeric? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_numeric")))

(define (GtkSpinButton-get-range self)
"  ARGS: 
     min  - real number of size gdouble[OUT], 
     max  - real number of size gdouble[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_range")))

(define (GtkSpinButton-get-snap-to-ticks? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_snap_to_ticks")))

(define (GtkSpinButton-get-update-policy self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_update_policy")))

(define (GtkSpinButton-get-value self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_value")))

(define (GtkSpinButton-get-value-as-int self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_value_as_int")))

(define (GtkSpinButton-get-wrap? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_wrap")))

(define (GtkSpinButton-set-adjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_adjustment" adjustment)))

(define (GtkSpinButton-set-digits self digits)
"  ARGS: 
     digits  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_digits" digits)))

(define (GtkSpinButton-set-increments self step page)
"  ARGS: 
     step  - real number of size gdouble, 
     page  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_increments" step page)))

(define (GtkSpinButton-set-numeric self numeric)
"  ARGS: 
     numeric  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_numeric" numeric)))

(define (GtkSpinButton-set-range self min max)
"  ARGS: 
     min  - real number of size gdouble, 
     max  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_range" min max)))

(define (GtkSpinButton-set-snap-to-ticks self snap-to-ticks)
"  ARGS: 
     snap-to-ticks  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_snap_to_ticks" snap-to-ticks)))

(define (GtkSpinButton-set-update-policy self policy)
"  ARGS: 
     policy  - exact integer of enum type SpinButtonUpdatePolicy
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_update_policy" policy)))

(define (GtkSpinButton-set-value self value)
"  ARGS: 
     value  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" value)))

(define (GtkSpinButton-set-wrap self wrap)
"  ARGS: 
     wrap  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wrap" wrap)))

(define (GtkSpinButton-spin self direction increment)
"  ARGS: 
     direction  - exact integer of enum type SpinType, 
     increment  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "spin" direction increment)))

(define (GtkSpinButton-update self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "update")))

(define <GtkSpinButtonAccessible>
  (gi-lookup-type "Gtk-SpinButtonAccessible"))

(define SPIN_BUTTON_UPDATE_POLICY_ALWAYS
  (gi-enum-value "Gtk-SpinButtonUpdatePolicy" "always"))

(define SPIN_BUTTON_UPDATE_POLICY_IF_VALID
  (gi-enum-value "Gtk-SpinButtonUpdatePolicy" "if_valid"))

(define SPIN_TYPE_STEP_FORWARD
  (gi-enum-value "Gtk-SpinType" "step_forward"))

(define SPIN_TYPE_STEP_BACKWARD
  (gi-enum-value "Gtk-SpinType" "step_backward"))

(define SPIN_TYPE_PAGE_FORWARD
  (gi-enum-value "Gtk-SpinType" "page_forward"))

(define SPIN_TYPE_PAGE_BACKWARD
  (gi-enum-value "Gtk-SpinType" "page_backward"))

(define SPIN_TYPE_HOME
  (gi-enum-value "Gtk-SpinType" "home"))

(define SPIN_TYPE_END
  (gi-enum-value "Gtk-SpinType" "end"))

(define SPIN_TYPE_USER_DEFINED
  (gi-enum-value "Gtk-SpinType" "user_defined"))

(define <GtkSpinner>
  (gi-lookup-type "Gtk-Spinner"))

(define (GtkSpinner-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Spinner-new"))

(define (GtkSpinner-start self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "start")))

(define (GtkSpinner-stop self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "stop")))

(define <GtkSpinnerAccessible>
  (gi-lookup-type "Gtk-SpinnerAccessible"))

(define <GtkStack>
  (gi-lookup-type "Gtk-Stack"))

(define (GtkStack-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Stack-new"))

(define (GtkStack-add-named self child name)
"  ARGS: 
     child  - object Widget, 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_named" child name)))

(define (GtkStack-add-titled self child name title)
"  ARGS: 
     child  - object Widget, 
     name  - string, 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_titled" child name title)))

(define (GtkStack-get-child-by-name self name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child_by_name" name)))

(define (GtkStack-get-hhomogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hhomogeneous")))

(define (GtkStack-get-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_homogeneous")))

(define (GtkStack-get-interpolate-size? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_interpolate_size")))

(define (GtkStack-get-transition-duration self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_transition_duration")))

(define (GtkStack-get-transition-running? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_transition_running")))

(define (GtkStack-get-transition-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_transition_type")))

(define (GtkStack-get-vhomogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_vhomogeneous")))

(define (GtkStack-get-visible-child self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_child")))

(define (GtkStack-get-visible-child-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_child_name")))

(define (GtkStack-set-hhomogeneous self hhomogeneous)
"  ARGS: 
     hhomogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hhomogeneous" hhomogeneous)))

(define (GtkStack-set-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_homogeneous" homogeneous)))

(define (GtkStack-set-interpolate-size self interpolate-size)
"  ARGS: 
     interpolate-size  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_interpolate_size" interpolate-size)))

(define (GtkStack-set-transition-duration self duration)
"  ARGS: 
     duration  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transition_duration" duration)))

(define (GtkStack-set-transition-type self transition)
"  ARGS: 
     transition  - exact integer of enum type StackTransitionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transition_type" transition)))

(define (GtkStack-set-vhomogeneous self vhomogeneous)
"  ARGS: 
     vhomogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vhomogeneous" vhomogeneous)))

(define (GtkStack-set-visible-child self child)
"  ARGS: 
     child  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_child" child)))

(define (GtkStack-set-visible-child-full self name transition)
"  ARGS: 
     name  - string, 
     transition  - exact integer of enum type StackTransitionType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_child_full" name transition)))

(define (GtkStack-set-visible-child-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_child_name" name)))

(define <GtkStackAccessible>
  (gi-lookup-type "Gtk-StackAccessible"))

(define <GtkStackSidebar>
  (gi-lookup-type "Gtk-StackSidebar"))

(define (GtkStackSidebar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StackSidebar-new"))

(define (GtkStackSidebar-get-stack self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_stack")))

(define (GtkStackSidebar-set-stack self stack)
"  ARGS: 
     stack  - object Stack
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_stack" stack)))

(define <GtkStackSwitcher>
  (gi-lookup-type "Gtk-StackSwitcher"))

(define (GtkStackSwitcher-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StackSwitcher-new"))

(define (GtkStackSwitcher-get-stack self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_stack")))

(define (GtkStackSwitcher-set-stack self stack)
"  ARGS: 
     stack  - object Stack
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_stack" stack)))

(define STACK_TRANSITION_TYPE_NONE
  (gi-enum-value "Gtk-StackTransitionType" "none"))

(define STACK_TRANSITION_TYPE_CROSSFADE
  (gi-enum-value "Gtk-StackTransitionType" "crossfade"))

(define STACK_TRANSITION_TYPE_SLIDE_RIGHT
  (gi-enum-value "Gtk-StackTransitionType" "slide_right"))

(define STACK_TRANSITION_TYPE_SLIDE_LEFT
  (gi-enum-value "Gtk-StackTransitionType" "slide_left"))

(define STACK_TRANSITION_TYPE_SLIDE_UP
  (gi-enum-value "Gtk-StackTransitionType" "slide_up"))

(define STACK_TRANSITION_TYPE_SLIDE_DOWN
  (gi-enum-value "Gtk-StackTransitionType" "slide_down"))

(define STACK_TRANSITION_TYPE_SLIDE_LEFT_RIGHT
  (gi-enum-value "Gtk-StackTransitionType" "slide_left_right"))

(define STACK_TRANSITION_TYPE_SLIDE_UP_DOWN
  (gi-enum-value "Gtk-StackTransitionType" "slide_up_down"))

(define STACK_TRANSITION_TYPE_OVER_UP
  (gi-enum-value "Gtk-StackTransitionType" "over_up"))

(define STACK_TRANSITION_TYPE_OVER_DOWN
  (gi-enum-value "Gtk-StackTransitionType" "over_down"))

(define STACK_TRANSITION_TYPE_OVER_LEFT
  (gi-enum-value "Gtk-StackTransitionType" "over_left"))

(define STACK_TRANSITION_TYPE_OVER_RIGHT
  (gi-enum-value "Gtk-StackTransitionType" "over_right"))

(define STACK_TRANSITION_TYPE_UNDER_UP
  (gi-enum-value "Gtk-StackTransitionType" "under_up"))

(define STACK_TRANSITION_TYPE_UNDER_DOWN
  (gi-enum-value "Gtk-StackTransitionType" "under_down"))

(define STACK_TRANSITION_TYPE_UNDER_LEFT
  (gi-enum-value "Gtk-StackTransitionType" "under_left"))

(define STACK_TRANSITION_TYPE_UNDER_RIGHT
  (gi-enum-value "Gtk-StackTransitionType" "under_right"))

(define STACK_TRANSITION_TYPE_OVER_UP_DOWN
  (gi-enum-value "Gtk-StackTransitionType" "over_up_down"))

(define STACK_TRANSITION_TYPE_OVER_DOWN_UP
  (gi-enum-value "Gtk-StackTransitionType" "over_down_up"))

(define STACK_TRANSITION_TYPE_OVER_LEFT_RIGHT
  (gi-enum-value "Gtk-StackTransitionType" "over_left_right"))

(define STACK_TRANSITION_TYPE_OVER_RIGHT_LEFT
  (gi-enum-value "Gtk-StackTransitionType" "over_right_left"))

(define NORMAL
  (gi-flag-value "Gtk-StateFlags" "normal"))

(define ACTIVE
  (gi-flag-value "Gtk-StateFlags" "active"))

(define PRELIGHT
  (gi-flag-value "Gtk-StateFlags" "prelight"))

(define SELECTED
  (gi-flag-value "Gtk-StateFlags" "selected"))

(define INSENSITIVE
  (gi-flag-value "Gtk-StateFlags" "insensitive"))

(define INCONSISTENT
  (gi-flag-value "Gtk-StateFlags" "inconsistent"))

(define FOCUSED
  (gi-flag-value "Gtk-StateFlags" "focused"))

(define BACKDROP
  (gi-flag-value "Gtk-StateFlags" "backdrop"))

(define DIR_LTR
  (gi-flag-value "Gtk-StateFlags" "dir_ltr"))

(define DIR_RTL
  (gi-flag-value "Gtk-StateFlags" "dir_rtl"))

(define LINK
  (gi-flag-value "Gtk-StateFlags" "link"))

(define VISITED
  (gi-flag-value "Gtk-StateFlags" "visited"))

(define CHECKED
  (gi-flag-value "Gtk-StateFlags" "checked"))

(define DROP_ACTIVE
  (gi-flag-value "Gtk-StateFlags" "drop_active"))

(define <GtkStatusIcon>
  (gi-lookup-type "Gtk-StatusIcon"))

(define (GtkStatusIcon-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new"))

(define (GtkStatusIcon-new-from-file filename)
"  ARGS: 
     filename  - locale string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new_from_file" filename))

(define (GtkStatusIcon-new-from-gicon icon)
"  ARGS: 
     icon  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new_from_gicon" icon))

(define (GtkStatusIcon-new-from-icon-name icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new_from_icon_name" icon-name))

(define (GtkStatusIcon-new-from-pixbuf pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new_from_pixbuf" pixbuf))

(define (GtkStatusIcon-new-from-stock stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StatusIcon-new_from_stock" stock-id))

(define (GtkStatusIcon-position-menu menu x y user-data)
"  ARGS: 
     menu  - object Menu, 
     x  - exact integer of size gint32[INOUT] , 
     y  - exact integer of size gint32[INOUT] , 
     push-in  - boolean[OUT], 
     user-data  - object StatusIcon
   RETURN: void
"
  (gi-function-invoke "Gtk-StatusIcon-position_menu" menu x y user-data))

(define (GtkStatusIcon-get-geometry? self out-area)
"  ARGS: 
     screen  - object Screen[OUT], 
     orientation  - exact integer of enum type Orientation[OUT]
   RETURN: gboolean
     area  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_geometry" out-area)))

(define (GtkStatusIcon-get-gicon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_gicon")))

(define (GtkStatusIcon-get-has-tooltip? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_tooltip")))

(define (GtkStatusIcon-get-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkStatusIcon-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkStatusIcon-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkStatusIcon-get-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkStatusIcon-get-stock self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_stock")))

(define (GtkStatusIcon-get-storage-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_storage_type")))

(define (GtkStatusIcon-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkStatusIcon-get-tooltip-markup self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_markup")))

(define (GtkStatusIcon-get-tooltip-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_text")))

(define (GtkStatusIcon-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkStatusIcon-get-x11-window-id self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_x11_window_id")))

(define (GtkStatusIcon-is-embedded? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_embedded")))

(define (GtkStatusIcon-set-from-file self filename)
"  ARGS: 
     filename  - locale string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_file" filename)))

(define (GtkStatusIcon-set-from-gicon self icon)
"  ARGS: 
     icon  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_gicon" icon)))

(define (GtkStatusIcon-set-from-icon-name self icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_icon_name" icon-name)))

(define (GtkStatusIcon-set-from-pixbuf self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_pixbuf" pixbuf)))

(define (GtkStatusIcon-set-from-stock self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_from_stock" stock-id)))

(define (GtkStatusIcon-set-has-tooltip self has-tooltip)
"  ARGS: 
     has-tooltip  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_tooltip" has-tooltip)))

(define (GtkStatusIcon-set-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_name" name)))

(define (GtkStatusIcon-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define (GtkStatusIcon-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkStatusIcon-set-tooltip-markup self markup)
"  ARGS: 
     markup  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_markup" markup)))

(define (GtkStatusIcon-set-tooltip-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_text" text)))

(define (GtkStatusIcon-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define <GtkStatusbar>
  (gi-lookup-type "Gtk-Statusbar"))

(define (GtkStatusbar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Statusbar-new"))

(define (GtkStatusbar-get-context-id self context-description)
"  ARGS: 
     context-description  - string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_context_id" context-description)))

(define (GtkStatusbar-get-message-area self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_message_area")))

(define (GtkStatusbar-pop self context-id)
"  ARGS: 
     context-id  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pop" context-id)))

(define (GtkStatusbar-push self context-id text)
"  ARGS: 
     context-id  - exact integer of size guint32, 
     text  - string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "push" context-id text)))

(define (GtkStatusbar-remove self context-id message-id)
"  ARGS: 
     context-id  - exact integer of size guint32, 
     message-id  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" context-id message-id)))

(define (GtkStatusbar-remove-all self context-id)
"  ARGS: 
     context-id  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_all" context-id)))

(define <GtkStatusbarAccessible>
  (gi-lookup-type "Gtk-StatusbarAccessible"))

(define <GtkStyle>
  (gi-lookup-type "Gtk-Style"))

(define (GtkStyle-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Style-new"))

(define (GtkStyle-apply-default-background self cr window state-type x y width height)
"  ARGS: 
     cr  - struct Context, 
     window  - object Window, 
     state-type  - exact integer of enum type StateType, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "apply_default_background" cr window state-type x y width height)))

(define (GtkStyle-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkStyle-detach self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "detach")))

(define (GtkStyle-get-style-property self widget-type property-name out-value)
"  ARGS: 
     widget-type  - <GType>, 
     property-name  - string, 
   RETURN: void
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_style_property" widget-type property-name out-value)))

(define (GtkStyle-has-context? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_context")))

(define (GtkStyle-lookup-color? self color-name out-color)
"  ARGS: 
     color-name  - string, 
   RETURN: gboolean
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "lookup_color" color-name out-color)))

(define (GtkStyle-lookup-icon-set self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_icon_set" stock-id)))

(define (GtkStyle-render-icon self source direction state size widget detail)
"  ARGS: 
     source  - struct IconSource, 
     direction  - exact integer of enum type TextDirection, 
     state  - exact integer of enum type StateType, 
     size  - exact integer of size gint32, 
     widget  - object Widget, 
     detail  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon" source direction state size widget detail)))

(define (GtkStyle-set-background self window state-type)
"  ARGS: 
     window  - object Window, 
     state-type  - exact integer of enum type StateType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background" window state-type)))

(define <GtkStyleContext>
  (gi-lookup-type "Gtk-StyleContext"))

(define (GtkStyleContext-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StyleContext-new"))

(define (GtkStyleContext-add-provider-for-screen screen provider priority)
"  ARGS: 
     screen  - object Screen, 
     provider  - Unhandled argument type tag 16, 
     priority  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-StyleContext-add_provider_for_screen" screen provider priority))

(define (GtkStyleContext-remove-provider-for-screen screen provider)
"  ARGS: 
     screen  - object Screen, 
     provider  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-function-invoke "Gtk-StyleContext-remove_provider_for_screen" screen provider))

(define (GtkStyleContext-reset-widgets screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-function-invoke "Gtk-StyleContext-reset_widgets" screen))

(define (GtkStyleContext-add-class self class-name)
"  ARGS: 
     class-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_class" class-name)))

(define (GtkStyleContext-add-provider self provider priority)
"  ARGS: 
     provider  - Unhandled argument type tag 16, 
     priority  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_provider" provider priority)))

(define (GtkStyleContext-add-region self region-name flags)
"  ARGS: 
     region-name  - string, 
     flags  - exact integer of flags type RegionFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_region" region-name flags)))

(define (GtkStyleContext-cancel-animations self region-id)
"  ARGS: 
     region-id  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cancel_animations" region-id)))

(define (GtkStyleContext-get-background-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_background_color" state out-color)))

(define (GtkStyleContext-get-border self state out-border)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     border  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_border" state out-border)))

(define (GtkStyleContext-get-border-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_border_color" state out-color)))

(define (GtkStyleContext-get-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_color" state out-color)))

(define (GtkStyleContext-get-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_direction")))

(define (GtkStyleContext-get-font self state)
"  ARGS: 
     state  - exact integer of flags type StateFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_font" state)))

(define (GtkStyleContext-get-frame-clock self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_frame_clock")))

(define (GtkStyleContext-get-junction-sides self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_junction_sides")))

(define (GtkStyleContext-get-margin self state out-margin)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     margin  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_margin" state out-margin)))

(define (GtkStyleContext-get-padding self state out-padding)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     padding  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_padding" state out-padding)))

(define (GtkStyleContext-get-parent self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent")))

(define (GtkStyleContext-get-path self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path")))

(define (GtkStyleContext-get-property self property state out-value)
"  ARGS: 
     property  - string, 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_property" property state out-value)))

(define (GtkStyleContext-get-scale self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_scale")))

(define (GtkStyleContext-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkStyleContext-get-section self property)
"  ARGS: 
     property  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_section" property)))

(define (GtkStyleContext-get-state self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state")))

(define (GtkStyleContext-get-style-property self property-name value)
"  ARGS: 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_style_property" property-name value)))

(define (GtkStyleContext-has-class? self class-name)
"  ARGS: 
     class-name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_class" class-name)))

(define (GtkStyleContext-has-region? self region-name)
"  ARGS: 
     region-name  - string, 
     flags-return  - exact integer of flags type RegionFlags[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_region" region-name)))

(define (GtkStyleContext-invalidate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "invalidate")))

(define (GtkStyleContext-list-classes self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_classes")))

(define (GtkStyleContext-list-regions self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_regions")))

(define (GtkStyleContext-lookup-color? self color-name out-color)
"  ARGS: 
     color-name  - string, 
   RETURN: gboolean
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "lookup_color" color-name out-color)))

(define (GtkStyleContext-lookup-icon-set self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_icon_set" stock-id)))

(define (GtkStyleContext-notify-state-change self window region-id state state-value)
"  ARGS: 
     window  - object Window, 
     region-id  - #f for NULL or pointer, 
     state  - exact integer of enum type StateType, 
     state-value  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "notify_state_change" window region-id state state-value)))

(define (GtkStyleContext-pop-animatable-region self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pop_animatable_region")))

(define (GtkStyleContext-push-animatable-region self region-id)
"  ARGS: 
     region-id  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "push_animatable_region" region-id)))

(define (GtkStyleContext-remove-class self class-name)
"  ARGS: 
     class-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_class" class-name)))

(define (GtkStyleContext-remove-provider self provider)
"  ARGS: 
     provider  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_provider" provider)))

(define (GtkStyleContext-remove-region self region-name)
"  ARGS: 
     region-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_region" region-name)))

(define (GtkStyleContext-restore self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "restore")))

(define (GtkStyleContext-save self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "save")))

(define (GtkStyleContext-scroll-animations self window dx dy)
"  ARGS: 
     window  - object Window, 
     dx  - exact integer of size gint32, 
     dy  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_animations" window dx dy)))

(define (GtkStyleContext-set-background self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_background" window)))

(define (GtkStyleContext-set-direction self direction)
"  ARGS: 
     direction  - exact integer of enum type TextDirection
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_direction" direction)))

(define (GtkStyleContext-set-frame-clock self frame-clock)
"  ARGS: 
     frame-clock  - object FrameClock
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_frame_clock" frame-clock)))

(define (GtkStyleContext-set-junction-sides self sides)
"  ARGS: 
     sides  - exact integer of flags type JunctionSides
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_junction_sides" sides)))

(define (GtkStyleContext-set-parent self parent)
"  ARGS: 
     parent  - object StyleContext
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_parent" parent)))

(define (GtkStyleContext-set-path self path)
"  ARGS: 
     path  - struct WidgetPath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_path" path)))

(define (GtkStyleContext-set-scale self scale)
"  ARGS: 
     scale  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_scale" scale)))

(define (GtkStyleContext-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define (GtkStyleContext-set-state self flags)
"  ARGS: 
     flags  - exact integer of flags type StateFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state" flags)))

(define (GtkStyleContext-state-is-running? self state)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     progress  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "state_is_running" state)))

(define (GtkStyleContext-to-string self flags)
"  ARGS: 
     flags  - exact integer of flags type StyleContextPrintFlags
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string" flags)))

(define NONE
  (gi-flag-value "Gtk-StyleContextPrintFlags" "none"))

(define RECURSE
  (gi-flag-value "Gtk-StyleContextPrintFlags" "recurse"))

(define SHOW_STYLE
  (gi-flag-value "Gtk-StyleContextPrintFlags" "show_style"))

(define <GtkStyleProperties>
  (gi-lookup-type "Gtk-StyleProperties"))

(define (GtkStyleProperties-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-StyleProperties-new"))

(define (GtkStyleProperties-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkStyleProperties-get-property? self property state out-value)
"  ARGS: 
     property  - string, 
     state  - exact integer of flags type StateFlags, 
   RETURN: gboolean
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_property" property state out-value)))

(define (GtkStyleProperties-lookup-color self name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup_color" name)))

(define (GtkStyleProperties-map-color self name color)
"  ARGS: 
     name  - string, 
     color  - struct SymbolicColor
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "map_color" name color)))

(define (GtkStyleProperties-merge self props-to-merge replace)
"  ARGS: 
     props-to-merge  - object StyleProperties, 
     replace  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "merge" props-to-merge replace)))

(define (GtkStyleProperties-set-property self property state value)
"  ARGS: 
     property  - string, 
     state  - exact integer of flags type StateFlags, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_property" property state value)))

(define (GtkStyleProperties-unset-property self property state)
"  ARGS: 
     property  - string, 
     state  - exact integer of flags type StateFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_property" property state)))

;; CALLBACK
(define style-property-parser
  (gi-lookup-callback-info "Gtk-StylePropertyParser"))
;; ARGS: 
;;   string  - string, 
;;   value  - struct Value
;; RETURN: gboolean
(define <GtkSwitch>
  (gi-lookup-type "Gtk-Switch"))

(define (GtkSwitch-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Switch-new"))

(define (GtkSwitch-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkSwitch-get-state? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_state")))

(define (GtkSwitch-set-active self is-active)
"  ARGS: 
     is-active  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" is-active)))

(define (GtkSwitch-set-state self state)
"  ARGS: 
     state  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state" state)))

(define <GtkSwitchAccessible>
  (gi-lookup-type "Gtk-SwitchAccessible"))

(define <GtkSymbolicColor>
  (gi-lookup-type "Gtk-SymbolicColor"))

(define (GtkSymbolicColor-new-alpha color factor)
"  ARGS: 
     color  - struct SymbolicColor, 
     factor  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_alpha" color factor))

(define (GtkSymbolicColor-new-literal color)
"  ARGS: 
     color  - struct RGBA
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_literal" color))

(define (GtkSymbolicColor-new-mix color1 color2 factor)
"  ARGS: 
     color1  - struct SymbolicColor, 
     color2  - struct SymbolicColor, 
     factor  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_mix" color1 color2 factor))

(define (GtkSymbolicColor-new-name name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_name" name))

(define (GtkSymbolicColor-new-shade color factor)
"  ARGS: 
     color  - struct SymbolicColor, 
     factor  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_shade" color factor))

(define (GtkSymbolicColor-new-win32 theme-class id)
"  ARGS: 
     theme-class  - string, 
     id  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-SymbolicColor-new_win32" theme-class id))

(define (GtkSymbolicColor-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkSymbolicColor-resolve? self props out-resolved-color)
"  ARGS: 
     props  - object StyleProperties, 
   RETURN: gboolean
     resolved-color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "resolve" props out-resolved-color)))

(define (GtkSymbolicColor-to-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string")))

(define (GtkSymbolicColor-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define GTK_TEXT_VIEW_PRIORITY_VALIDATE
  (gi-constant-value "Gtk-TEXT_VIEW_PRIORITY_VALIDATE"))

(define GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
  (gi-constant-value "Gtk-TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID"))

(define GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
  (gi-constant-value "Gtk-TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID"))

(define <GtkTable>
  (gi-lookup-type "Gtk-Table"))

(define (GtkTable-new rows columns homogeneous)
"  ARGS: 
     rows  - exact integer of size guint32, 
     columns  - exact integer of size guint32, 
     homogeneous  - boolean
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Table-new" rows columns homogeneous))

(define (GtkTable-attach self child left-attach right-attach top-attach bottom-attach xoptions yoptions xpadding ypadding)
"  ARGS: 
     child  - object Widget, 
     left-attach  - exact integer of size guint32, 
     right-attach  - exact integer of size guint32, 
     top-attach  - exact integer of size guint32, 
     bottom-attach  - exact integer of size guint32, 
     xoptions  - exact integer of flags type AttachOptions, 
     yoptions  - exact integer of flags type AttachOptions, 
     xpadding  - exact integer of size guint32, 
     ypadding  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach" child left-attach right-attach top-attach bottom-attach xoptions yoptions xpadding ypadding)))

(define (GtkTable-attach-defaults self widget left-attach right-attach top-attach bottom-attach)
"  ARGS: 
     widget  - object Widget, 
     left-attach  - exact integer of size guint32, 
     right-attach  - exact integer of size guint32, 
     top-attach  - exact integer of size guint32, 
     bottom-attach  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "attach_defaults" widget left-attach right-attach top-attach bottom-attach)))

(define (GtkTable-get-col-spacing self column)
"  ARGS: 
     column  - exact integer of size guint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_col_spacing" column)))

(define (GtkTable-get-default-col-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_default_col_spacing")))

(define (GtkTable-get-default-row-spacing self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_default_row_spacing")))

(define (GtkTable-get-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_homogeneous")))

(define (GtkTable-get-row-spacing self row)
"  ARGS: 
     row  - exact integer of size guint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_row_spacing" row)))

(define (GtkTable-get-size self)
"  ARGS: 
     rows  - exact integer of size guint32[OUT], 
     columns  - exact integer of size guint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkTable-resize self rows columns)
"  ARGS: 
     rows  - exact integer of size guint32, 
     columns  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "resize" rows columns)))

(define (GtkTable-set-col-spacing self column spacing)
"  ARGS: 
     column  - exact integer of size guint32, 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_col_spacing" column spacing)))

(define (GtkTable-set-col-spacings self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_col_spacings" spacing)))

(define (GtkTable-set-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_homogeneous" homogeneous)))

(define (GtkTable-set-row-spacing self row spacing)
"  ARGS: 
     row  - exact integer of size guint32, 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_spacing" row spacing)))

(define (GtkTable-set-row-spacings self spacing)
"  ARGS: 
     spacing  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_spacings" spacing)))

(define <GtkTargetEntry>
  (gi-lookup-type "Gtk-TargetEntry"))

(define (GtkTargetEntry-new target flags info)
"  ARGS: 
     target  - string, 
     flags  - exact integer of size guint32, 
     info  - exact integer of size guint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TargetEntry-new" target flags info))

(define (GtkTargetEntry-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTargetEntry-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define SAME_APP
  (gi-flag-value "Gtk-TargetFlags" "same_app"))

(define SAME_WIDGET
  (gi-flag-value "Gtk-TargetFlags" "same_widget"))

(define OTHER_APP
  (gi-flag-value "Gtk-TargetFlags" "other_app"))

(define OTHER_WIDGET
  (gi-flag-value "Gtk-TargetFlags" "other_widget"))

(define <GtkTargetList>
  (gi-lookup-type "Gtk-TargetList"))

(define (GtkTargetList-new targets ntargets)
"  ARGS: 
     targets  - #f for NULL or Unhandled argument type tag 15, 
     ntargets  - exact integer of size guint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TargetList-new" targets ntargets))

(define (GtkTargetList-add self target flags info)
"  ARGS: 
     target  - struct Atom, 
     flags  - exact integer of size guint32, 
     info  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add" target flags info)))

(define (GtkTargetList-add-image-targets self info writable)
"  ARGS: 
     info  - exact integer of size guint32, 
     writable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_image_targets" info writable)))

(define (GtkTargetList-add-rich-text-targets self info deserializable buffer)
"  ARGS: 
     info  - exact integer of size guint32, 
     deserializable  - boolean, 
     buffer  - object TextBuffer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_rich_text_targets" info deserializable buffer)))

(define (GtkTargetList-add-table self targets ntargets)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     ntargets  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_table" targets ntargets)))

(define (GtkTargetList-add-text-targets self info)
"  ARGS: 
     info  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_text_targets" info)))

(define (GtkTargetList-add-uri-targets self info)
"  ARGS: 
     info  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_uri_targets" info)))

(define (GtkTargetList-find? self target)
"  ARGS: 
     target  - struct Atom, 
     info  - exact integer of size guint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "find" target)))

(define (GtkTargetList-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkTargetList-remove self target)
"  ARGS: 
     target  - struct Atom
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" target)))

(define (GtkTargetList-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define <GtkTearoffMenuItem>
  (gi-lookup-type "Gtk-TearoffMenuItem"))

(define (GtkTearoffMenuItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TearoffMenuItem-new"))

(define <GtkTextAttributes>
  (gi-lookup-type "Gtk-TextAttributes"))

(define (GtkTextAttributes-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextAttributes-new"))

(define (GtkTextAttributes-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTextAttributes-copy-values self dest)
"  ARGS: 
     dest  - struct TextAttributes
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "copy_values" dest)))

(define (GtkTextAttributes-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkTextAttributes-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define <GtkTextBuffer>
  (gi-lookup-type "Gtk-TextBuffer"))

(define (GtkTextBuffer-new table)
"  ARGS: 
     table  - object TextTagTable
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextBuffer-new" table))

(define (GtkTextBuffer-add-mark self mark where)
"  ARGS: 
     mark  - object TextMark, 
     where  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mark" mark where)))

(define (GtkTextBuffer-add-selection-clipboard self clipboard)
"  ARGS: 
     clipboard  - object Clipboard
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_selection_clipboard" clipboard)))

(define (GtkTextBuffer-apply-tag self tag start end)
"  ARGS: 
     tag  - object TextTag, 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "apply_tag" tag start end)))

(define (GtkTextBuffer-apply-tag-by-name self name start end)
"  ARGS: 
     name  - string, 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "apply_tag_by_name" name start end)))

(define (GtkTextBuffer-backspace? self iter interactive default-editable)
"  ARGS: 
     iter  - struct TextIter, 
     interactive  - boolean, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backspace" iter interactive default-editable)))

(define (GtkTextBuffer-begin-user-action self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "begin_user_action")))

(define (GtkTextBuffer-copy-clipboard self clipboard)
"  ARGS: 
     clipboard  - object Clipboard
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "copy_clipboard" clipboard)))

(define (GtkTextBuffer-create-child-anchor self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_child_anchor" iter)))

(define (GtkTextBuffer-create-mark self mark-name where left-gravity)
"  ARGS: 
     mark-name  - #f for NULL or string, 
     where  - struct TextIter, 
     left-gravity  - boolean
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_mark" mark-name where left-gravity)))

(define (GtkTextBuffer-cut-clipboard self clipboard default-editable)
"  ARGS: 
     clipboard  - object Clipboard, 
     default-editable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cut_clipboard" clipboard default-editable)))

(define (GtkTextBuffer-delete self start end)
"  ARGS: 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "delete" start end)))

(define (GtkTextBuffer-delete-interactive? self start-iter end-iter default-editable)
"  ARGS: 
     start-iter  - struct TextIter, 
     end-iter  - struct TextIter, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "delete_interactive" start-iter end-iter default-editable)))

(define (GtkTextBuffer-delete-mark self mark)
"  ARGS: 
     mark  - object TextMark
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "delete_mark" mark)))

(define (GtkTextBuffer-delete-mark-by-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "delete_mark_by_name" name)))

(define (GtkTextBuffer-delete-selection? self interactive default-editable)
"  ARGS: 
     interactive  - boolean, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "delete_selection" interactive default-editable)))

(define (GtkTextBuffer-deserialize? self content-buffer format iter data length)
"  ARGS: 
     content-buffer  - object TextBuffer, 
     format  - struct Atom, 
     iter  - struct TextIter, 
     data  - Unhandled argument type tag 15, 
     length  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "deserialize" content-buffer format iter data length)))

(define (GtkTextBuffer-deserialize-get-can-create-tags? self format)
"  ARGS: 
     format  - struct Atom
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "deserialize_get_can_create_tags" format)))

(define (GtkTextBuffer-deserialize-set-can-create-tags self format can-create-tags)
"  ARGS: 
     format  - struct Atom, 
     can-create-tags  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "deserialize_set_can_create_tags" format can-create-tags)))

(define (GtkTextBuffer-end-user-action self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "end_user_action")))

(define (GtkTextBuffer-get-bounds self out-start out-end)
"  ARGS: 
   RETURN: void
     start  - Unhandled argument type tag 16, 
     end  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_bounds" out-start out-end)))

(define (GtkTextBuffer-get-char-count self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_char_count")))

(define (GtkTextBuffer-get-copy-target-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_copy_target_list")))

(define (GtkTextBuffer-get-deserialize-formats self)
"  ARGS: 
     n-formats  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_deserialize_formats")))

(define (GtkTextBuffer-get-end-iter self out-iter)
"  ARGS: 
   RETURN: void
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_end_iter" out-iter)))

(define (GtkTextBuffer-get-has-selection? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_selection")))

(define (GtkTextBuffer-get-insert self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_insert")))

(define (GtkTextBuffer-get-iter-at-child-anchor self out-iter anchor)
"  ARGS: 
     anchor  - object TextChildAnchor
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_child_anchor" out-iter anchor)))

(define (GtkTextBuffer-get-iter-at-line self out-iter line-number)
"  ARGS: 
     line-number  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_line" out-iter line-number)))

(define (GtkTextBuffer-get-iter-at-line-index self out-iter line-number byte-index)
"  ARGS: 
     line-number  - exact integer of size gint32, 
     byte-index  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_line_index" out-iter line-number byte-index)))

(define (GtkTextBuffer-get-iter-at-line-offset self out-iter line-number char-offset)
"  ARGS: 
     line-number  - exact integer of size gint32, 
     char-offset  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_line_offset" out-iter line-number char-offset)))

(define (GtkTextBuffer-get-iter-at-mark self out-iter mark)
"  ARGS: 
     mark  - object TextMark
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_mark" out-iter mark)))

(define (GtkTextBuffer-get-iter-at-offset self out-iter char-offset)
"  ARGS: 
     char-offset  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_offset" out-iter char-offset)))

(define (GtkTextBuffer-get-line-count self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_line_count")))

(define (GtkTextBuffer-get-mark self name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_mark" name)))

(define (GtkTextBuffer-get-modified? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_modified")))

(define (GtkTextBuffer-get-paste-target-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_paste_target_list")))

(define (GtkTextBuffer-get-selection-bound self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_bound")))

(define (GtkTextBuffer-get-selection-bounds? self out-start out-end)
"  ARGS: 
   RETURN: gboolean
     start  - Unhandled argument type tag 16, 
     end  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_selection_bounds" out-start out-end)))

(define (GtkTextBuffer-get-serialize-formats self)
"  ARGS: 
     n-formats  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_serialize_formats")))

(define (GtkTextBuffer-get-slice self start end include-hidden-chars)
"  ARGS: 
     start  - struct TextIter, 
     end  - struct TextIter, 
     include-hidden-chars  - boolean
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_slice" start end include-hidden-chars)))

(define (GtkTextBuffer-get-start-iter self out-iter)
"  ARGS: 
   RETURN: void
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_start_iter" out-iter)))

(define (GtkTextBuffer-get-tag-table self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tag_table")))

(define (GtkTextBuffer-get-text self start end include-hidden-chars)
"  ARGS: 
     start  - struct TextIter, 
     end  - struct TextIter, 
     include-hidden-chars  - boolean
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text" start end include-hidden-chars)))

(define (GtkTextBuffer-insert self iter text len)
"  ARGS: 
     iter  - struct TextIter, 
     text  - string, 
     len  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" iter text len)))

(define (GtkTextBuffer-insert-at-cursor self text len)
"  ARGS: 
     text  - string, 
     len  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_at_cursor" text len)))

(define (GtkTextBuffer-insert-child-anchor self iter anchor)
"  ARGS: 
     iter  - struct TextIter, 
     anchor  - object TextChildAnchor
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_child_anchor" iter anchor)))

(define (GtkTextBuffer-insert-interactive? self iter text len default-editable)
"  ARGS: 
     iter  - struct TextIter, 
     text  - string, 
     len  - exact integer of size gint32, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "insert_interactive" iter text len default-editable)))

(define (GtkTextBuffer-insert-interactive-at-cursor? self text len default-editable)
"  ARGS: 
     text  - string, 
     len  - exact integer of size gint32, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "insert_interactive_at_cursor" text len default-editable)))

(define (GtkTextBuffer-insert-markup self iter markup len)
"  ARGS: 
     iter  - struct TextIter, 
     markup  - string, 
     len  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_markup" iter markup len)))

(define (GtkTextBuffer-insert-pixbuf self iter pixbuf)
"  ARGS: 
     iter  - struct TextIter, 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_pixbuf" iter pixbuf)))

(define (GtkTextBuffer-insert-range self iter start end)
"  ARGS: 
     iter  - struct TextIter, 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_range" iter start end)))

(define (GtkTextBuffer-insert-range-interactive? self iter start end default-editable)
"  ARGS: 
     iter  - struct TextIter, 
     start  - struct TextIter, 
     end  - struct TextIter, 
     default-editable  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "insert_range_interactive" iter start end default-editable)))

(define (GtkTextBuffer-move-mark self mark where)
"  ARGS: 
     mark  - object TextMark, 
     where  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_mark" mark where)))

(define (GtkTextBuffer-move-mark-by-name self name where)
"  ARGS: 
     name  - string, 
     where  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_mark_by_name" name where)))

(define (GtkTextBuffer-paste-clipboard self clipboard override-location default-editable)
"  ARGS: 
     clipboard  - object Clipboard, 
     override-location  - struct TextIter, 
     default-editable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "paste_clipboard" clipboard override-location default-editable)))

(define (GtkTextBuffer-place-cursor self where)
"  ARGS: 
     where  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "place_cursor" where)))

(define (GtkTextBuffer-register-deserialize-format self mime-type function user-data user-data-destroy)
"  ARGS: 
     mime-type  - string, 
     function  - procedure of type TextBufferDeserializeFunc, 
     user-data  - #f for NULL or pointer, 
     user-data-destroy  - procedure of type DestroyNotify
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "register_deserialize_format" mime-type function user-data user-data-destroy)))

(define (GtkTextBuffer-register-deserialize-tagset self tagset-name)
"  ARGS: 
     tagset-name  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "register_deserialize_tagset" tagset-name)))

(define (GtkTextBuffer-register-serialize-format self mime-type function user-data user-data-destroy)
"  ARGS: 
     mime-type  - string, 
     function  - procedure of type TextBufferSerializeFunc, 
     user-data  - #f for NULL or pointer, 
     user-data-destroy  - procedure of type DestroyNotify
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "register_serialize_format" mime-type function user-data user-data-destroy)))

(define (GtkTextBuffer-register-serialize-tagset self tagset-name)
"  ARGS: 
     tagset-name  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "register_serialize_tagset" tagset-name)))

(define (GtkTextBuffer-remove-all-tags self start end)
"  ARGS: 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_all_tags" start end)))

(define (GtkTextBuffer-remove-selection-clipboard self clipboard)
"  ARGS: 
     clipboard  - object Clipboard
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_selection_clipboard" clipboard)))

(define (GtkTextBuffer-remove-tag self tag start end)
"  ARGS: 
     tag  - object TextTag, 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_tag" tag start end)))

(define (GtkTextBuffer-remove-tag-by-name self name start end)
"  ARGS: 
     name  - string, 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_tag_by_name" name start end)))

(define (GtkTextBuffer-select-range self ins bound)
"  ARGS: 
     ins  - struct TextIter, 
     bound  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_range" ins bound)))

(define (GtkTextBuffer-serialize self content-buffer format start end)
"  ARGS: 
     content-buffer  - object TextBuffer, 
     format  - struct Atom, 
     start  - struct TextIter, 
     end  - struct TextIter, 
     length  - exact integer of size guint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "serialize" content-buffer format start end)))

(define (GtkTextBuffer-set-modified self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_modified" setting)))

(define (GtkTextBuffer-set-text self text len)
"  ARGS: 
     text  - string, 
     len  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" text len)))

(define (GtkTextBuffer-unregister-deserialize-format self format)
"  ARGS: 
     format  - struct Atom
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unregister_deserialize_format" format)))

(define (GtkTextBuffer-unregister-serialize-format self format)
"  ARGS: 
     format  - struct Atom
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unregister_serialize_format" format)))

;; CALLBACK
(define text-buffer-deserialize-func
  (gi-lookup-callback-info "Gtk-TextBufferDeserializeFunc"))
;; ARGS: 
;;   register-buffer  - object TextBuffer, 
;;   content-buffer  - object TextBuffer, 
;;   iter  - struct TextIter, 
;;   data  - Unhandled argument type tag 15, 
;;   length  - exact integer of size guint32, 
;;   create-tags  - boolean, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define text-buffer-serialize-func
  (gi-lookup-callback-info "Gtk-TextBufferSerializeFunc"))
;; ARGS: 
;;   register-buffer  - object TextBuffer, 
;;   content-buffer  - object TextBuffer, 
;;   start  - struct TextIter, 
;;   end  - struct TextIter, 
;;   length  - bytevector containing elements guint32, 
;;   user-data  - #f for NULL or pointer
;; RETURN: guint8*
(define TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS
  (gi-enum-value "Gtk-TextBufferTargetInfo" "buffer_contents"))

(define TEXT_BUFFER_TARGET_INFO_RICH_TEXT
  (gi-enum-value "Gtk-TextBufferTargetInfo" "rich_text"))

(define TEXT_BUFFER_TARGET_INFO_TEXT
  (gi-enum-value "Gtk-TextBufferTargetInfo" "text"))

(define <GtkTextCellAccessible>
  (gi-lookup-type "Gtk-TextCellAccessible"))

;; CALLBACK
(define text-char-predicate
  (gi-lookup-callback-info "Gtk-TextCharPredicate"))
;; ARGS: 
;;   ch  - character, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkTextChildAnchor>
  (gi-lookup-type "Gtk-TextChildAnchor"))

(define (GtkTextChildAnchor-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextChildAnchor-new"))

(define (GtkTextChildAnchor-get-deleted? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_deleted")))

(define (GtkTextChildAnchor-get-widgets self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_widgets")))

(define TEXT_DIRECTION_NONE
  (gi-enum-value "Gtk-TextDirection" "none"))

(define TEXT_DIRECTION_LTR
  (gi-enum-value "Gtk-TextDirection" "ltr"))

(define TEXT_DIRECTION_RTL
  (gi-enum-value "Gtk-TextDirection" "rtl"))

(define TEXT_EXTEND_SELECTION_WORD
  (gi-enum-value "Gtk-TextExtendSelection" "word"))

(define TEXT_EXTEND_SELECTION_LINE
  (gi-enum-value "Gtk-TextExtendSelection" "line"))

(define <GtkTextIter>
  (gi-lookup-type "Gtk-TextIter"))

(define (GtkTextIter-assign self other)
"  ARGS: 
     other  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "assign" other)))

(define (GtkTextIter-backward-char? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_char")))

(define (GtkTextIter-backward-chars? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_chars" count)))

(define (GtkTextIter-backward-cursor-position? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_cursor_position")))

(define (GtkTextIter-backward-cursor-positions? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_cursor_positions" count)))

(define (GtkTextIter-backward-find-char? self pred user-data limit)
"  ARGS: 
     pred  - procedure of type TextCharPredicate, 
     user-data  - #f for NULL or pointer, 
     limit  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_find_char" pred user-data limit)))

(define (GtkTextIter-backward-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_line")))

(define (GtkTextIter-backward-lines? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_lines" count)))

(define (GtkTextIter-backward-search? self str flags out-match-start out-match-end limit)
"  ARGS: 
     str  - string, 
     flags  - exact integer of flags type TextSearchFlags, 
     limit  - struct TextIter
   RETURN: gboolean
     match-start  - Unhandled argument type tag 16, 
     match-end  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "backward_search" str flags out-match-start out-match-end limit)))

(define (GtkTextIter-backward-sentence-start? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_sentence_start")))

(define (GtkTextIter-backward-sentence-starts? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_sentence_starts" count)))

(define (GtkTextIter-backward-to-tag-toggle? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_to_tag_toggle" tag)))

(define (GtkTextIter-backward-visible-cursor-position? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_cursor_position")))

(define (GtkTextIter-backward-visible-cursor-positions? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_cursor_positions" count)))

(define (GtkTextIter-backward-visible-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_line")))

(define (GtkTextIter-backward-visible-lines? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_lines" count)))

(define (GtkTextIter-backward-visible-word-start? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_word_start")))

(define (GtkTextIter-backward-visible-word-starts? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_visible_word_starts" count)))

(define (GtkTextIter-backward-word-start? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_word_start")))

(define (GtkTextIter-backward-word-starts? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_word_starts" count)))

(define (GtkTextIter-begins-tag? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "begins_tag" tag)))

(define (GtkTextIter-can-insert? self default-editability)
"  ARGS: 
     default-editability  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "can_insert" default-editability)))

(define (GtkTextIter-compare self rhs)
"  ARGS: 
     rhs  - struct TextIter
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "compare" rhs)))

(define (GtkTextIter-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTextIter-editable? self default-setting)
"  ARGS: 
     default-setting  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "editable" default-setting)))

(define (GtkTextIter-ends-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "ends_line")))

(define (GtkTextIter-ends-sentence? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "ends_sentence")))

(define (GtkTextIter-ends-tag? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "ends_tag" tag)))

(define (GtkTextIter-ends-word? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "ends_word")))

(define (GtkTextIter-equal? self rhs)
"  ARGS: 
     rhs  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "equal" rhs)))

(define (GtkTextIter-forward-char? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_char")))

(define (GtkTextIter-forward-chars? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_chars" count)))

(define (GtkTextIter-forward-cursor-position? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_cursor_position")))

(define (GtkTextIter-forward-cursor-positions? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_cursor_positions" count)))

(define (GtkTextIter-forward-find-char? self pred user-data limit)
"  ARGS: 
     pred  - procedure of type TextCharPredicate, 
     user-data  - #f for NULL or pointer, 
     limit  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_find_char" pred user-data limit)))

(define (GtkTextIter-forward-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_line")))

(define (GtkTextIter-forward-lines? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_lines" count)))

(define (GtkTextIter-forward-search? self str flags out-match-start out-match-end limit)
"  ARGS: 
     str  - string, 
     flags  - exact integer of flags type TextSearchFlags, 
     limit  - struct TextIter
   RETURN: gboolean
     match-start  - Unhandled argument type tag 16, 
     match-end  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "forward_search" str flags out-match-start out-match-end limit)))

(define (GtkTextIter-forward-sentence-end? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_sentence_end")))

(define (GtkTextIter-forward-sentence-ends? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_sentence_ends" count)))

(define (GtkTextIter-forward-to-end self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "forward_to_end")))

(define (GtkTextIter-forward-to-line-end? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_to_line_end")))

(define (GtkTextIter-forward-to-tag-toggle? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_to_tag_toggle" tag)))

(define (GtkTextIter-forward-visible-cursor-position? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_cursor_position")))

(define (GtkTextIter-forward-visible-cursor-positions? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_cursor_positions" count)))

(define (GtkTextIter-forward-visible-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_line")))

(define (GtkTextIter-forward-visible-lines? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_lines" count)))

(define (GtkTextIter-forward-visible-word-end? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_word_end")))

(define (GtkTextIter-forward-visible-word-ends? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_visible_word_ends" count)))

(define (GtkTextIter-forward-word-end? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_word_end")))

(define (GtkTextIter-forward-word-ends? self count)
"  ARGS: 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_word_ends" count)))

(define (GtkTextIter-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkTextIter-get-attributes? self out-values)
"  ARGS: 
   RETURN: gboolean
     values  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_attributes" out-values)))

(define (GtkTextIter-get-buffer self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_buffer")))

(define (GtkTextIter-get-bytes-in-line self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_bytes_in_line")))

(define (GtkTextIter-get-char self)
"  ARGS: 
   RETURN: gunichar
"
  (gi-method-send self 
     (gi-method-prepare "get_char")))

(define (GtkTextIter-get-chars-in-line self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_chars_in_line")))

(define (GtkTextIter-get-child-anchor self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_child_anchor")))

(define (GtkTextIter-get-language self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_language")))

(define (GtkTextIter-get-line self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_line")))

(define (GtkTextIter-get-line-index self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_line_index")))

(define (GtkTextIter-get-line-offset self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_line_offset")))

(define (GtkTextIter-get-marks self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_marks")))

(define (GtkTextIter-get-offset self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_offset")))

(define (GtkTextIter-get-pixbuf self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pixbuf")))

(define (GtkTextIter-get-slice self end)
"  ARGS: 
     end  - struct TextIter
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_slice" end)))

(define (GtkTextIter-get-tags self)
"  ARGS: 
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_tags")))

(define (GtkTextIter-get-text self end)
"  ARGS: 
     end  - struct TextIter
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_text" end)))

(define (GtkTextIter-get-toggled-tags self toggled-on)
"  ARGS: 
     toggled-on  - boolean
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_toggled_tags" toggled-on)))

(define (GtkTextIter-get-visible-line-index self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_line_index")))

(define (GtkTextIter-get-visible-line-offset self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_line_offset")))

(define (GtkTextIter-get-visible-slice self end)
"  ARGS: 
     end  - struct TextIter
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_slice" end)))

(define (GtkTextIter-get-visible-text self end)
"  ARGS: 
     end  - struct TextIter
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_text" end)))

(define (GtkTextIter-has-tag? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_tag" tag)))

(define (GtkTextIter-in-range? self start end)
"  ARGS: 
     start  - struct TextIter, 
     end  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "in_range" start end)))

(define (GtkTextIter-inside-sentence? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "inside_sentence")))

(define (GtkTextIter-inside-word? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "inside_word")))

(define (GtkTextIter-is-cursor-position? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_cursor_position")))

(define (GtkTextIter-is-end? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_end")))

(define (GtkTextIter-is-start? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_start")))

(define (GtkTextIter-order self second)
"  ARGS: 
     second  - struct TextIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "order" second)))

(define (GtkTextIter-set-line self line-number)
"  ARGS: 
     line-number  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_line" line-number)))

(define (GtkTextIter-set-line-index self byte-on-line)
"  ARGS: 
     byte-on-line  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_line_index" byte-on-line)))

(define (GtkTextIter-set-line-offset self char-on-line)
"  ARGS: 
     char-on-line  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_line_offset" char-on-line)))

(define (GtkTextIter-set-offset self char-offset)
"  ARGS: 
     char-offset  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_offset" char-offset)))

(define (GtkTextIter-set-visible-line-index self byte-on-line)
"  ARGS: 
     byte-on-line  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_line_index" byte-on-line)))

(define (GtkTextIter-set-visible-line-offset self char-on-line)
"  ARGS: 
     char-on-line  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_line_offset" char-on-line)))

(define (GtkTextIter-starts-line? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "starts_line")))

(define (GtkTextIter-starts-sentence? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "starts_sentence")))

(define (GtkTextIter-starts-tag? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "starts_tag" tag)))

(define (GtkTextIter-starts-word? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "starts_word")))

(define (GtkTextIter-toggles-tag? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "toggles_tag" tag)))

(define <GtkTextMark>
  (gi-lookup-type "Gtk-TextMark"))

(define (GtkTextMark-new name left-gravity)
"  ARGS: 
     name  - #f for NULL or string, 
     left-gravity  - boolean
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextMark-new" name left-gravity))

(define (GtkTextMark-get-buffer self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_buffer")))

(define (GtkTextMark-get-deleted? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_deleted")))

(define (GtkTextMark-get-left-gravity? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_left_gravity")))

(define (GtkTextMark-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkTextMark-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkTextMark-set-visible self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" setting)))

(define VISIBLE_ONLY
  (gi-flag-value "Gtk-TextSearchFlags" "visible_only"))

(define TEXT_ONLY
  (gi-flag-value "Gtk-TextSearchFlags" "text_only"))

(define CASE_INSENSITIVE
  (gi-flag-value "Gtk-TextSearchFlags" "case_insensitive"))

(define <GtkTextTag>
  (gi-lookup-type "Gtk-TextTag"))

(define (GtkTextTag-new name)
"  ARGS: 
     name  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextTag-new" name))

(define (GtkTextTag-changed self size-changed)
"  ARGS: 
     size-changed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "changed" size-changed)))

(define (GtkTextTag-event? self event-object event iter)
"  ARGS: 
     event-object  - object Object, 
     event  - union Event, 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "event" event-object event iter)))

(define (GtkTextTag-get-priority self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_priority")))

(define (GtkTextTag-set-priority self priority)
"  ARGS: 
     priority  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_priority" priority)))

(define <GtkTextTagTable>
  (gi-lookup-type "Gtk-TextTagTable"))

(define (GtkTextTagTable-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextTagTable-new"))

(define (GtkTextTagTable-add? self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "add" tag)))

(define (GtkTextTagTable-foreach self func data)
"  ARGS: 
     func  - procedure of type TextTagTableForeach, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "foreach" func data)))

(define (GtkTextTagTable-get-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkTextTagTable-lookup self name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "lookup" name)))

(define (GtkTextTagTable-remove self tag)
"  ARGS: 
     tag  - object TextTag
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove" tag)))

;; CALLBACK
(define text-tag-table-foreach
  (gi-lookup-callback-info "Gtk-TextTagTableForeach"))
;; ARGS: 
;;   tag  - object TextTag, 
;;   data  - #f for NULL or pointer
;; RETURN: void
(define <GtkTextView>
  (gi-lookup-type "Gtk-TextView"))

(define (GtkTextView-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextView-new"))

(define (GtkTextView-new-with-buffer buffer)
"  ARGS: 
     buffer  - object TextBuffer
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TextView-new_with_buffer" buffer))

(define (GtkTextView-add-child-at-anchor self child anchor)
"  ARGS: 
     child  - object Widget, 
     anchor  - object TextChildAnchor
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_child_at_anchor" child anchor)))

(define (GtkTextView-add-child-in-window self child which-window xpos ypos)
"  ARGS: 
     child  - object Widget, 
     which-window  - exact integer of enum type TextWindowType, 
     xpos  - exact integer of size gint32, 
     ypos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_child_in_window" child which-window xpos ypos)))

(define (GtkTextView-backward-display-line? self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_display_line" iter)))

(define (GtkTextView-backward-display-line-start? self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "backward_display_line_start" iter)))

(define (GtkTextView-buffer-to-window-coords self win buffer-x buffer-y)
"  ARGS: 
     win  - exact integer of enum type TextWindowType, 
     buffer-x  - exact integer of size gint32, 
     buffer-y  - exact integer of size gint32, 
     window-x  - exact integer of size gint32[OUT], 
     window-y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "buffer_to_window_coords" win buffer-x buffer-y)))

(define (GtkTextView-forward-display-line? self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_display_line" iter)))

(define (GtkTextView-forward-display-line-end? self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "forward_display_line_end" iter)))

(define (GtkTextView-get-accepts-tab? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_accepts_tab")))

(define (GtkTextView-get-border-window-size self type)
"  ARGS: 
     type  - exact integer of enum type TextWindowType
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_border_window_size" type)))

(define (GtkTextView-get-bottom-margin self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_bottom_margin")))

(define (GtkTextView-get-buffer self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_buffer")))

(define (GtkTextView-get-cursor-locations self iter out-strong out-weak)
"  ARGS: 
     iter  - struct TextIter, 
   RETURN: void
     strong  - Unhandled argument type tag 16, 
     weak  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_cursor_locations" iter out-strong out-weak)))

(define (GtkTextView-get-cursor-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_cursor_visible")))

(define (GtkTextView-get-default-attributes self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_attributes")))

(define (GtkTextView-get-editable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_editable")))

(define (GtkTextView-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkTextView-get-indent self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_indent")))

(define (GtkTextView-get-input-hints self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_input_hints")))

(define (GtkTextView-get-input-purpose self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_input_purpose")))

(define (GtkTextView-get-iter-at-location? self out-iter x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: gboolean
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_location" out-iter x y)))

(define (GtkTextView-get-iter-at-position? self out-iter x y)
"  ARGS: 
     trailing  - exact integer of size gint32[OUT], 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: gboolean
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_at_position" out-iter x y)))

(define (GtkTextView-get-iter-location self iter out-location)
"  ARGS: 
     iter  - struct TextIter, 
   RETURN: void
     location  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_iter_location" iter out-location)))

(define (GtkTextView-get-justification self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_justification")))

(define (GtkTextView-get-left-margin self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_left_margin")))

(define (GtkTextView-get-line-at-y self out-target-iter y)
"  ARGS: 
     y  - exact integer of size gint32, 
     line-top  - exact integer of size gint32[OUT]
   RETURN: void
     target-iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_line_at_y" out-target-iter y)))

(define (GtkTextView-get-line-yrange self iter)
"  ARGS: 
     iter  - struct TextIter, 
     y  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_line_yrange" iter)))

(define (GtkTextView-get-monospace? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_monospace")))

(define (GtkTextView-get-overwrite? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_overwrite")))

(define (GtkTextView-get-pixels-above-lines self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_pixels_above_lines")))

(define (GtkTextView-get-pixels-below-lines self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_pixels_below_lines")))

(define (GtkTextView-get-pixels-inside-wrap self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_pixels_inside_wrap")))

(define (GtkTextView-get-right-margin self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_right_margin")))

(define (GtkTextView-get-tabs self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tabs")))

(define (GtkTextView-get-top-margin self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_top_margin")))

(define (GtkTextView-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkTextView-get-visible-rect self out-visible-rect)
"  ARGS: 
   RETURN: void
     visible-rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_rect" out-visible-rect)))

(define (GtkTextView-get-window self win)
"  ARGS: 
     win  - exact integer of enum type TextWindowType
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_window" win)))

(define (GtkTextView-get-window-type self window)
"  ARGS: 
     window  - object Window
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_window_type" window)))

(define (GtkTextView-get-wrap-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_wrap_mode")))

(define (GtkTextView-im-context-filter-keypress? self event)
"  ARGS: 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "im_context_filter_keypress" event)))

(define (GtkTextView-move-child self child xpos ypos)
"  ARGS: 
     child  - object Widget, 
     xpos  - exact integer of size gint32, 
     ypos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_child" child xpos ypos)))

(define (GtkTextView-move-mark-onscreen? self mark)
"  ARGS: 
     mark  - object TextMark
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "move_mark_onscreen" mark)))

(define (GtkTextView-move-visually? self iter count)
"  ARGS: 
     iter  - struct TextIter, 
     count  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "move_visually" iter count)))

(define (GtkTextView-place-cursor-onscreen? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "place_cursor_onscreen")))

(define (GtkTextView-reset-cursor-blink self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_cursor_blink")))

(define (GtkTextView-reset-im-context self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_im_context")))

(define (GtkTextView-scroll-mark-onscreen self mark)
"  ARGS: 
     mark  - object TextMark
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_mark_onscreen" mark)))

(define (GtkTextView-scroll-to-iter? self iter within-margin use-align xalign yalign)
"  ARGS: 
     iter  - struct TextIter, 
     within-margin  - real number of size gdouble, 
     use-align  - boolean, 
     xalign  - real number of size gdouble, 
     yalign  - real number of size gdouble
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "scroll_to_iter" iter within-margin use-align xalign yalign)))

(define (GtkTextView-scroll-to-mark self mark within-margin use-align xalign yalign)
"  ARGS: 
     mark  - object TextMark, 
     within-margin  - real number of size gdouble, 
     use-align  - boolean, 
     xalign  - real number of size gdouble, 
     yalign  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_to_mark" mark within-margin use-align xalign yalign)))

(define (GtkTextView-set-accepts-tab self accepts-tab)
"  ARGS: 
     accepts-tab  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accepts_tab" accepts-tab)))

(define (GtkTextView-set-border-window-size self type size)
"  ARGS: 
     type  - exact integer of enum type TextWindowType, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_border_window_size" type size)))

(define (GtkTextView-set-bottom-margin self bottom-margin)
"  ARGS: 
     bottom-margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_bottom_margin" bottom-margin)))

(define (GtkTextView-set-buffer self buffer)
"  ARGS: 
     buffer  - object TextBuffer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_buffer" buffer)))

(define (GtkTextView-set-cursor-visible self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor_visible" setting)))

(define (GtkTextView-set-editable self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_editable" setting)))

(define (GtkTextView-set-indent self indent)
"  ARGS: 
     indent  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_indent" indent)))

(define (GtkTextView-set-input-hints self hints)
"  ARGS: 
     hints  - exact integer of flags type InputHints
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_input_hints" hints)))

(define (GtkTextView-set-input-purpose self purpose)
"  ARGS: 
     purpose  - exact integer of enum type InputPurpose
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_input_purpose" purpose)))

(define (GtkTextView-set-justification self justification)
"  ARGS: 
     justification  - exact integer of enum type Justification
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_justification" justification)))

(define (GtkTextView-set-left-margin self left-margin)
"  ARGS: 
     left-margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_left_margin" left-margin)))

(define (GtkTextView-set-monospace self monospace)
"  ARGS: 
     monospace  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_monospace" monospace)))

(define (GtkTextView-set-overwrite self overwrite)
"  ARGS: 
     overwrite  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_overwrite" overwrite)))

(define (GtkTextView-set-pixels-above-lines self pixels-above-lines)
"  ARGS: 
     pixels-above-lines  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixels_above_lines" pixels-above-lines)))

(define (GtkTextView-set-pixels-below-lines self pixels-below-lines)
"  ARGS: 
     pixels-below-lines  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixels_below_lines" pixels-below-lines)))

(define (GtkTextView-set-pixels-inside-wrap self pixels-inside-wrap)
"  ARGS: 
     pixels-inside-wrap  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_pixels_inside_wrap" pixels-inside-wrap)))

(define (GtkTextView-set-right-margin self right-margin)
"  ARGS: 
     right-margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_right_margin" right-margin)))

(define (GtkTextView-set-tabs self tabs)
"  ARGS: 
     tabs  - struct TabArray
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tabs" tabs)))

(define (GtkTextView-set-top-margin self top-margin)
"  ARGS: 
     top-margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_top_margin" top-margin)))

(define (GtkTextView-set-wrap-mode self wrap-mode)
"  ARGS: 
     wrap-mode  - exact integer of enum type WrapMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wrap_mode" wrap-mode)))

(define (GtkTextView-starts-display-line? self iter)
"  ARGS: 
     iter  - struct TextIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "starts_display_line" iter)))

(define (GtkTextView-window-to-buffer-coords self win window-x window-y)
"  ARGS: 
     win  - exact integer of enum type TextWindowType, 
     window-x  - exact integer of size gint32, 
     window-y  - exact integer of size gint32, 
     buffer-x  - exact integer of size gint32[OUT], 
     buffer-y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "window_to_buffer_coords" win window-x window-y)))

(define <GtkTextViewAccessible>
  (gi-lookup-type "Gtk-TextViewAccessible"))

(define TEXT_VIEW_LAYER_BELOW
  (gi-enum-value "Gtk-TextViewLayer" "below"))

(define TEXT_VIEW_LAYER_ABOVE
  (gi-enum-value "Gtk-TextViewLayer" "above"))

(define TEXT_VIEW_LAYER_BELOW_TEXT
  (gi-enum-value "Gtk-TextViewLayer" "below_text"))

(define TEXT_VIEW_LAYER_ABOVE_TEXT
  (gi-enum-value "Gtk-TextViewLayer" "above_text"))

(define TEXT_WINDOW_TYPE_PRIVATE
  (gi-enum-value "Gtk-TextWindowType" "private"))

(define TEXT_WINDOW_TYPE_WIDGET
  (gi-enum-value "Gtk-TextWindowType" "widget"))

(define TEXT_WINDOW_TYPE_TEXT
  (gi-enum-value "Gtk-TextWindowType" "text"))

(define TEXT_WINDOW_TYPE_LEFT
  (gi-enum-value "Gtk-TextWindowType" "left"))

(define TEXT_WINDOW_TYPE_RIGHT
  (gi-enum-value "Gtk-TextWindowType" "right"))

(define TEXT_WINDOW_TYPE_TOP
  (gi-enum-value "Gtk-TextWindowType" "top"))

(define TEXT_WINDOW_TYPE_BOTTOM
  (gi-enum-value "Gtk-TextWindowType" "bottom"))

(define <GtkThemingEngine>
  (gi-lookup-type "Gtk-ThemingEngine"))

(define (GtkThemingEngine-load name)
"  ARGS: 
     name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ThemingEngine-load" name))

(define (GtkThemingEngine-get-background-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_background_color" state out-color)))

(define (GtkThemingEngine-get-border self state out-border)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     border  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_border" state out-border)))

(define (GtkThemingEngine-get-border-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_border_color" state out-color)))

(define (GtkThemingEngine-get-color self state out-color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_color" state out-color)))

(define (GtkThemingEngine-get-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_direction")))

(define (GtkThemingEngine-get-font self state)
"  ARGS: 
     state  - exact integer of flags type StateFlags
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_font" state)))

(define (GtkThemingEngine-get-junction-sides self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_junction_sides")))

(define (GtkThemingEngine-get-margin self state out-margin)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     margin  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_margin" state out-margin)))

(define (GtkThemingEngine-get-padding self state out-padding)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     padding  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_padding" state out-padding)))

(define (GtkThemingEngine-get-path self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path")))

(define (GtkThemingEngine-get-property self property state out-value)
"  ARGS: 
     property  - string, 
     state  - exact integer of flags type StateFlags, 
   RETURN: void
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_property" property state out-value)))

(define (GtkThemingEngine-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkThemingEngine-get-state self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state")))

(define (GtkThemingEngine-get-style-property self property-name out-value)
"  ARGS: 
     property-name  - string, 
   RETURN: void
     value  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_style_property" property-name out-value)))

(define (GtkThemingEngine-has-class? self style-class)
"  ARGS: 
     style-class  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_class" style-class)))

(define (GtkThemingEngine-has-region? self style-region)
"  ARGS: 
     style-region  - string, 
     flags  - exact integer of flags type RegionFlags[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_region" style-region)))

(define (GtkThemingEngine-lookup-color? self color-name out-color)
"  ARGS: 
     color-name  - string, 
   RETURN: gboolean
     color  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "lookup_color" color-name out-color)))

(define (GtkThemingEngine-state-is-running? self state)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     progress  - real number of size gdouble[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "state_is_running" state)))

;; CALLBACK
(define tick-callback
  (gi-lookup-callback-info "Gtk-TickCallback"))
;; ARGS: 
;;   widget  - object Widget, 
;;   frame-clock  - object FrameClock, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkToggleAction>
  (gi-lookup-type "Gtk-ToggleAction"))

(define (GtkToggleAction-new name label tooltip stock-id)
"  ARGS: 
     name  - string, 
     label  - #f for NULL or string, 
     tooltip  - #f for NULL or string, 
     stock-id  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleAction-new" name label tooltip stock-id))

(define (GtkToggleAction-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkToggleAction-get-draw-as-radio? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_draw_as_radio")))

(define (GtkToggleAction-set-active self is-active)
"  ARGS: 
     is-active  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" is-active)))

(define (GtkToggleAction-set-draw-as-radio self draw-as-radio)
"  ARGS: 
     draw-as-radio  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_draw_as_radio" draw-as-radio)))

(define (GtkToggleAction-toggled self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toggled")))

(define <GtkToggleButton>
  (gi-lookup-type "Gtk-ToggleButton"))

(define (GtkToggleButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleButton-new"))

(define (GtkToggleButton-new-with-label label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleButton-new_with_label" label))

(define (GtkToggleButton-new-with-mnemonic label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleButton-new_with_mnemonic" label))

(define (GtkToggleButton-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkToggleButton-get-inconsistent? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_inconsistent")))

(define (GtkToggleButton-get-mode? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_mode")))

(define (GtkToggleButton-set-active self is-active)
"  ARGS: 
     is-active  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" is-active)))

(define (GtkToggleButton-set-inconsistent self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_inconsistent" setting)))

(define (GtkToggleButton-set-mode self draw-indicator)
"  ARGS: 
     draw-indicator  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mode" draw-indicator)))

(define (GtkToggleButton-toggled self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toggled")))

(define <GtkToggleButtonAccessible>
  (gi-lookup-type "Gtk-ToggleButtonAccessible"))

(define <GtkToggleToolButton>
  (gi-lookup-type "Gtk-ToggleToolButton"))

(define (GtkToggleToolButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleToolButton-new"))

(define (GtkToggleToolButton-new-from-stock stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToggleToolButton-new_from_stock" stock-id))

(define (GtkToggleToolButton-get-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_active")))

(define (GtkToggleToolButton-set-active self is-active)
"  ARGS: 
     is-active  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_active" is-active)))

(define <GtkToolButton>
  (gi-lookup-type "Gtk-ToolButton"))

(define (GtkToolButton-new icon-widget label)
"  ARGS: 
     icon-widget  - object Widget, 
     label  - #f for NULL or string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolButton-new" icon-widget label))

(define (GtkToolButton-new-from-stock stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolButton-new_from_stock" stock-id))

(define (GtkToolButton-get-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkToolButton-get-icon-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_widget")))

(define (GtkToolButton-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkToolButton-get-label-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_label_widget")))

(define (GtkToolButton-get-stock-id self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_stock_id")))

(define (GtkToolButton-get-use-underline? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_underline")))

(define (GtkToolButton-set-icon-name self icon-name)
"  ARGS: 
     icon-name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_name" icon-name)))

(define (GtkToolButton-set-icon-widget self icon-widget)
"  ARGS: 
     icon-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_widget" icon-widget)))

(define (GtkToolButton-set-label self label)
"  ARGS: 
     label  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkToolButton-set-label-widget self label-widget)
"  ARGS: 
     label-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_widget" label-widget)))

(define (GtkToolButton-set-stock-id self stock-id)
"  ARGS: 
     stock-id  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_stock_id" stock-id)))

(define (GtkToolButton-set-use-underline self use-underline)
"  ARGS: 
     use-underline  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_underline" use-underline)))

(define <GtkToolItem>
  (gi-lookup-type "Gtk-ToolItem"))

(define (GtkToolItem-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolItem-new"))

(define (GtkToolItem-get-ellipsize-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_ellipsize_mode")))

(define (GtkToolItem-get-expand? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_expand")))

(define (GtkToolItem-get-homogeneous? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_homogeneous")))

(define (GtkToolItem-get-icon-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_size")))

(define (GtkToolItem-get-is-important? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_is_important")))

(define (GtkToolItem-get-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_orientation")))

(define (GtkToolItem-get-proxy-menu-item self menu-item-id)
"  ARGS: 
     menu-item-id  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_proxy_menu_item" menu-item-id)))

(define (GtkToolItem-get-relief-style self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_relief_style")))

(define (GtkToolItem-get-text-alignment self)
"  ARGS: 
   RETURN: gfloat
"
  (gi-method-send self 
     (gi-method-prepare "get_text_alignment")))

(define (GtkToolItem-get-text-orientation self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_text_orientation")))

(define (GtkToolItem-get-text-size-group self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_text_size_group")))

(define (GtkToolItem-get-toolbar-style self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_toolbar_style")))

(define (GtkToolItem-get-use-drag-window? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_use_drag_window")))

(define (GtkToolItem-get-visible-horizontal? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_horizontal")))

(define (GtkToolItem-get-visible-vertical? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_vertical")))

(define (GtkToolItem-rebuild-menu self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "rebuild_menu")))

(define (GtkToolItem-retrieve-proxy-menu-item self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "retrieve_proxy_menu_item")))

(define (GtkToolItem-set-expand self expand)
"  ARGS: 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_expand" expand)))

(define (GtkToolItem-set-homogeneous self homogeneous)
"  ARGS: 
     homogeneous  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_homogeneous" homogeneous)))

(define (GtkToolItem-set-is-important self is-important)
"  ARGS: 
     is-important  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_is_important" is-important)))

(define (GtkToolItem-set-proxy-menu-item self menu-item-id menu-item)
"  ARGS: 
     menu-item-id  - string, 
     menu-item  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_proxy_menu_item" menu-item-id menu-item)))

(define (GtkToolItem-set-tooltip-markup self markup)
"  ARGS: 
     markup  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_markup" markup)))

(define (GtkToolItem-set-tooltip-text self text)
"  ARGS: 
     text  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_text" text)))

(define (GtkToolItem-set-use-drag-window self use-drag-window)
"  ARGS: 
     use-drag-window  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_use_drag_window" use-drag-window)))

(define (GtkToolItem-set-visible-horizontal self visible-horizontal)
"  ARGS: 
     visible-horizontal  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_horizontal" visible-horizontal)))

(define (GtkToolItem-set-visible-vertical self visible-vertical)
"  ARGS: 
     visible-vertical  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_vertical" visible-vertical)))

(define (GtkToolItem-toolbar-reconfigured self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "toolbar_reconfigured")))

(define <GtkToolItemGroup>
  (gi-lookup-type "Gtk-ToolItemGroup"))

(define (GtkToolItemGroup-new label)
"  ARGS: 
     label  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolItemGroup-new" label))

(define (GtkToolItemGroup-get-collapsed? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_collapsed")))

(define (GtkToolItemGroup-get-drop-item self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_drop_item" x y)))

(define (GtkToolItemGroup-get-ellipsize self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_ellipsize")))

(define (GtkToolItemGroup-get-header-relief self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_header_relief")))

(define (GtkToolItemGroup-get-item-position self item)
"  ARGS: 
     item  - object ToolItem
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_position" item)))

(define (GtkToolItemGroup-get-label self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_label")))

(define (GtkToolItemGroup-get-label-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_label_widget")))

(define (GtkToolItemGroup-get-n-items self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_items")))

(define (GtkToolItemGroup-get-nth-item self index)
"  ARGS: 
     index  - exact integer of size guint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_nth_item" index)))

(define (GtkToolItemGroup-insert self item position)
"  ARGS: 
     item  - object ToolItem, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" item position)))

(define (GtkToolItemGroup-set-collapsed self collapsed)
"  ARGS: 
     collapsed  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_collapsed" collapsed)))

(define (GtkToolItemGroup-set-ellipsize self ellipsize)
"  ARGS: 
     ellipsize  - exact integer of enum type EllipsizeMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_ellipsize" ellipsize)))

(define (GtkToolItemGroup-set-header-relief self style)
"  ARGS: 
     style  - exact integer of enum type ReliefStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_header_relief" style)))

(define (GtkToolItemGroup-set-item-position self item position)
"  ARGS: 
     item  - object ToolItem, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_item_position" item position)))

(define (GtkToolItemGroup-set-label self label)
"  ARGS: 
     label  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label" label)))

(define (GtkToolItemGroup-set-label-widget self label-widget)
"  ARGS: 
     label-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_label_widget" label-widget)))

(define <GtkToolPalette>
  (gi-lookup-type "Gtk-ToolPalette"))

(define (GtkToolPalette-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolPalette-new"))

(define (GtkToolPalette-get-drag-target-group)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolPalette-get_drag_target_group"))

(define (GtkToolPalette-get-drag-target-item)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-ToolPalette-get_drag_target_item"))

(define (GtkToolPalette-add-drag-dest self widget flags targets actions)
"  ARGS: 
     widget  - object Widget, 
     flags  - exact integer of flags type DestDefaults, 
     targets  - exact integer of flags type ToolPaletteDragTargets, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_drag_dest" widget flags targets actions)))

(define (GtkToolPalette-get-drag-item self selection)
"  ARGS: 
     selection  - struct SelectionData
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_drag_item" selection)))

(define (GtkToolPalette-get-drop-group self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_drop_group" x y)))

(define (GtkToolPalette-get-drop-item self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_drop_item" x y)))

(define (GtkToolPalette-get-exclusive? self group)
"  ARGS: 
     group  - object ToolItemGroup
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_exclusive" group)))

(define (GtkToolPalette-get-expand? self group)
"  ARGS: 
     group  - object ToolItemGroup
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_expand" group)))

(define (GtkToolPalette-get-group-position self group)
"  ARGS: 
     group  - object ToolItemGroup
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_group_position" group)))

(define (GtkToolPalette-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkToolPalette-get-icon-size self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_size")))

(define (GtkToolPalette-get-style self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_style")))

(define (GtkToolPalette-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkToolPalette-set-drag-source self targets)
"  ARGS: 
     targets  - exact integer of flags type ToolPaletteDragTargets
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_drag_source" targets)))

(define (GtkToolPalette-set-exclusive self group exclusive)
"  ARGS: 
     group  - object ToolItemGroup, 
     exclusive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_exclusive" group exclusive)))

(define (GtkToolPalette-set-expand self group expand)
"  ARGS: 
     group  - object ToolItemGroup, 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_expand" group expand)))

(define (GtkToolPalette-set-group-position self group position)
"  ARGS: 
     group  - object ToolItemGroup, 
     position  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_group_position" group position)))

(define (GtkToolPalette-set-icon-size self icon-size)
"  ARGS: 
     icon-size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_size" icon-size)))

(define (GtkToolPalette-set-style self style)
"  ARGS: 
     style  - exact integer of enum type ToolbarStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_style" style)))

(define (GtkToolPalette-unset-icon-size self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_icon_size")))

(define (GtkToolPalette-unset-style self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_style")))

(define ITEMS
  (gi-flag-value "Gtk-ToolPaletteDragTargets" "items"))

(define GROUPS
  (gi-flag-value "Gtk-ToolPaletteDragTargets" "groups"))

(define <GtkToolbar>
  (gi-lookup-type "Gtk-Toolbar"))

(define (GtkToolbar-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Toolbar-new"))

(define (GtkToolbar-get-drop-index self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_drop_index" x y)))

(define (GtkToolbar-get-icon-size self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_size")))

(define (GtkToolbar-get-item-index self item)
"  ARGS: 
     item  - object ToolItem
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_item_index" item)))

(define (GtkToolbar-get-n-items self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_items")))

(define (GtkToolbar-get-nth-item self n)
"  ARGS: 
     n  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_nth_item" n)))

(define (GtkToolbar-get-relief-style self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_relief_style")))

(define (GtkToolbar-get-show-arrow? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_arrow")))

(define (GtkToolbar-get-style self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_style")))

(define (GtkToolbar-insert self item pos)
"  ARGS: 
     item  - object ToolItem, 
     pos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert" item pos)))

(define (GtkToolbar-set-drop-highlight-item self tool-item index-)
"  ARGS: 
     tool-item  - object ToolItem, 
     index-  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_drop_highlight_item" tool-item index-)))

(define (GtkToolbar-set-icon-size self icon-size)
"  ARGS: 
     icon-size  - exact integer of enum type IconSize
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_size" icon-size)))

(define (GtkToolbar-set-show-arrow self show-arrow)
"  ARGS: 
     show-arrow  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_arrow" show-arrow)))

(define (GtkToolbar-set-style self style)
"  ARGS: 
     style  - exact integer of enum type ToolbarStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_style" style)))

(define (GtkToolbar-unset-icon-size self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_icon_size")))

(define (GtkToolbar-unset-style self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_style")))

(define TOOLBAR_STYLE_ICONS
  (gi-enum-value "Gtk-ToolbarStyle" "icons"))

(define TOOLBAR_STYLE_TEXT
  (gi-enum-value "Gtk-ToolbarStyle" "text"))

(define TOOLBAR_STYLE_BOTH
  (gi-enum-value "Gtk-ToolbarStyle" "both"))

(define TOOLBAR_STYLE_BOTH_HORIZ
  (gi-enum-value "Gtk-ToolbarStyle" "both_horiz"))

(define <GtkTooltip>
  (gi-lookup-type "Gtk-Tooltip"))

(define (GtkTooltip-trigger-tooltip-query display)
"  ARGS: 
     display  - object Display
   RETURN: void
"
  (gi-function-invoke "Gtk-Tooltip-trigger_tooltip_query" display))

(define (GtkTooltip-set-custom self custom-widget)
"  ARGS: 
     custom-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_custom" custom-widget)))

(define (GtkTooltip-set-icon self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon" pixbuf)))

(define (GtkTooltip-set-icon-from-gicon self gicon size)
"  ARGS: 
     gicon  - #f for NULL or Unhandled argument type tag 16, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_gicon" gicon size)))

(define (GtkTooltip-set-icon-from-icon-name self icon-name size)
"  ARGS: 
     icon-name  - #f for NULL or string, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_icon_name" icon-name size)))

(define (GtkTooltip-set-icon-from-stock self stock-id size)
"  ARGS: 
     stock-id  - #f for NULL or string, 
     size  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_stock" stock-id size)))

(define (GtkTooltip-set-markup self markup)
"  ARGS: 
     markup  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_markup" markup)))

(define (GtkTooltip-set-text self text)
"  ARGS: 
     text  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_text" text)))

(define (GtkTooltip-set-tip-area self rect)
"  ARGS: 
     rect  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tip_area" rect)))

(define <GtkToplevelAccessible>
  (gi-lookup-type "Gtk-ToplevelAccessible"))

(define (GtkToplevelAccessible-get-children self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_children")))

;; CALLBACK
(define tree-cell-data-func
  (gi-lookup-callback-info "Gtk-TreeCellDataFunc"))
;; ARGS: 
;;   tree-column  - object TreeViewColumn, 
;;   cell  - object CellRenderer, 
;;   tree-model  - Unhandled argument type tag 16, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define tree-destroy-count-func
  (gi-lookup-callback-info "Gtk-TreeDestroyCountFunc"))
;; ARGS: 
;;   tree-view  - object TreeView, 
;;   path  - struct TreePath, 
;;   children  - exact integer of size gint32, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define <GtkTreeIter>
  (gi-lookup-type "Gtk-TreeIter"))

(define (GtkTreeIter-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTreeIter-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

;; CALLBACK
(define tree-iter-compare-func
  (gi-lookup-callback-info "Gtk-TreeIterCompareFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   a  - struct TreeIter, 
;;   b  - struct TreeIter, 
;;   user-data  - #f for NULL or pointer
;; RETURN: gint32
(define <GtkTreeModelFilter>
  (gi-lookup-type "Gtk-TreeModelFilter"))

(define (GtkTreeModelFilter-clear-cache self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear_cache")))

(define (GtkTreeModelFilter-convert-child-iter-to-iter? self out-filter-iter child-iter)
"  ARGS: 
     child-iter  - struct TreeIter
   RETURN: gboolean
     filter-iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "convert_child_iter_to_iter" out-filter-iter child-iter)))

(define (GtkTreeModelFilter-convert-child-path-to-path self child-path)
"  ARGS: 
     child-path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "convert_child_path_to_path" child-path)))

(define (GtkTreeModelFilter-convert-iter-to-child-iter self out-child-iter filter-iter)
"  ARGS: 
     filter-iter  - struct TreeIter
   RETURN: void
     child-iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "convert_iter_to_child_iter" out-child-iter filter-iter)))

(define (GtkTreeModelFilter-convert-path-to-child-path self filter-path)
"  ARGS: 
     filter-path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "convert_path_to_child_path" filter-path)))

(define (GtkTreeModelFilter-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkTreeModelFilter-refilter self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "refilter")))

(define (GtkTreeModelFilter-set-modify-func self n-columns types func data destroy)
"  ARGS: 
     n-columns  - exact integer of size gint32, 
     types  - Unhandled argument type tag 15, 
     func  - procedure of type TreeModelFilterModifyFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_modify_func" n-columns types func data destroy)))

(define (GtkTreeModelFilter-set-visible-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_column" column)))

(define (GtkTreeModelFilter-set-visible-func self func data destroy)
"  ARGS: 
     func  - procedure of type TreeModelFilterVisibleFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible_func" func data destroy)))

;; CALLBACK
(define tree-model-filter-modify-func
  (gi-lookup-callback-info "Gtk-TreeModelFilterModifyFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   iter  - struct TreeIter, 
;;   column  - exact integer of size gint32, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;;   value  - Unhandled argument type tag 16, 
;; CALLBACK
(define tree-model-filter-visible-func
  (gi-lookup-callback-info "Gtk-TreeModelFilterVisibleFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define ITERS_PERSIST
  (gi-flag-value "Gtk-TreeModelFlags" "iters_persist"))

(define LIST_ONLY
  (gi-flag-value "Gtk-TreeModelFlags" "list_only"))

;; CALLBACK
(define tree-model-foreach-func
  (gi-lookup-callback-info "Gtk-TreeModelForeachFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   path  - struct TreePath, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkTreeModelSort>
  (gi-lookup-type "Gtk-TreeModelSort"))

(define (GtkTreeModelSort-clear-cache self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear_cache")))

(define (GtkTreeModelSort-convert-child-iter-to-iter? self out-sort-iter child-iter)
"  ARGS: 
     child-iter  - struct TreeIter
   RETURN: gboolean
     sort-iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "convert_child_iter_to_iter" out-sort-iter child-iter)))

(define (GtkTreeModelSort-convert-child-path-to-path self child-path)
"  ARGS: 
     child-path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "convert_child_path_to_path" child-path)))

(define (GtkTreeModelSort-convert-iter-to-child-iter self out-child-iter sorted-iter)
"  ARGS: 
     sorted-iter  - struct TreeIter
   RETURN: void
     child-iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "convert_iter_to_child_iter" out-child-iter sorted-iter)))

(define (GtkTreeModelSort-convert-path-to-child-path self sorted-path)
"  ARGS: 
     sorted-path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "convert_path_to_child_path" sorted-path)))

(define (GtkTreeModelSort-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkTreeModelSort-iter-is-valid? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_is_valid" iter)))

(define (GtkTreeModelSort-reset-default-sort-func self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_default_sort_func")))

(define <GtkTreePath>
  (gi-lookup-type "Gtk-TreePath"))

(define (GtkTreePath-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreePath-new"))

(define (GtkTreePath-new-first)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreePath-new_first"))

(define (GtkTreePath-new-from-indices indices length)
"  ARGS: 
     indices  - Unhandled argument type tag 15, 
     length  - exact integer of size guint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreePath-new_from_indices" indices length))

(define (GtkTreePath-new-from-string path)
"  ARGS: 
     path  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreePath-new_from_string" path))

(define (GtkTreePath-append-index self index-)
"  ARGS: 
     index-  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "append_index" index-)))

(define (GtkTreePath-compare self b)
"  ARGS: 
     b  - struct TreePath
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "compare" b)))

(define (GtkTreePath-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTreePath-down self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "down")))

(define (GtkTreePath-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkTreePath-get-depth self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_depth")))

(define (GtkTreePath-get-indices self)
"  ARGS: 
     depth  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "get_indices")))

(define (GtkTreePath-is-ancestor? self descendant)
"  ARGS: 
     descendant  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_ancestor" descendant)))

(define (GtkTreePath-is-descendant? self ancestor)
"  ARGS: 
     ancestor  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_descendant" ancestor)))

(define (GtkTreePath-next self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "next")))

(define (GtkTreePath-prepend-index self index-)
"  ARGS: 
     index-  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend_index" index-)))

(define (GtkTreePath-prev? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "prev")))

(define (GtkTreePath-to-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string")))

(define (GtkTreePath-up? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "up")))

(define <GtkTreeRowReference>
  (gi-lookup-type "Gtk-TreeRowReference"))

(define (GtkTreeRowReference-new model path)
"  ARGS: 
     model  - Unhandled argument type tag 16, 
     path  - struct TreePath
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeRowReference-new" model path))

(define (GtkTreeRowReference-new-proxy proxy model path)
"  ARGS: 
     proxy  - object Object, 
     model  - Unhandled argument type tag 16, 
     path  - struct TreePath
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeRowReference-new_proxy" proxy model path))

(define (GtkTreeRowReference-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkTreeRowReference-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkTreeRowReference-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkTreeRowReference-get-path self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path")))

(define (GtkTreeRowReference-valid? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "valid")))

(define (GtkTreeRowReference-deleted proxy path)
"  ARGS: 
     proxy  - object Object, 
     path  - struct TreePath
   RETURN: void
"
  (gi-function-invoke "Gtk-TreeRowReference-deleted" proxy path))

(define (GtkTreeRowReference-inserted proxy path)
"  ARGS: 
     proxy  - object Object, 
     path  - struct TreePath
   RETURN: void
"
  (gi-function-invoke "Gtk-TreeRowReference-inserted" proxy path))

(define <GtkTreeSelection>
  (gi-lookup-type "Gtk-TreeSelection"))

(define (GtkTreeSelection-count-selected-rows self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "count_selected_rows")))

(define (GtkTreeSelection-get-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_mode")))

(define (GtkTreeSelection-get-selected? self out-iter)
"  ARGS: 
     model  - Unhandled argument type tag 16[OUT], 
   RETURN: gboolean
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_selected" out-iter)))

(define (GtkTreeSelection-get-selected-rows self)
"  ARGS: 
     model  - Unhandled argument type tag 16[OUT]
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_selected_rows")))

(define (GtkTreeSelection-get-tree-view self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tree_view")))

(define (GtkTreeSelection-iter-is-selected? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_is_selected" iter)))

(define (GtkTreeSelection-path-is-selected? self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "path_is_selected" path)))

(define (GtkTreeSelection-select-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_all")))

(define (GtkTreeSelection-select-iter self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_iter" iter)))

(define (GtkTreeSelection-select-path self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_path" path)))

(define (GtkTreeSelection-select-range self start-path end-path)
"  ARGS: 
     start-path  - struct TreePath, 
     end-path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "select_range" start-path end-path)))

(define (GtkTreeSelection-selected-foreach self func data)
"  ARGS: 
     func  - procedure of type TreeSelectionForeachFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "selected_foreach" func data)))

(define (GtkTreeSelection-set-mode self type)
"  ARGS: 
     type  - exact integer of enum type SelectionMode
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mode" type)))

(define (GtkTreeSelection-set-select-function self func data destroy)
"  ARGS: 
     func  - procedure of type TreeSelectionFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_select_function" func data destroy)))

(define (GtkTreeSelection-unselect-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_all")))

(define (GtkTreeSelection-unselect-iter self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_iter" iter)))

(define (GtkTreeSelection-unselect-path self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_path" path)))

(define (GtkTreeSelection-unselect-range self start-path end-path)
"  ARGS: 
     start-path  - struct TreePath, 
     end-path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unselect_range" start-path end-path)))

;; CALLBACK
(define tree-selection-foreach-func
  (gi-lookup-callback-info "Gtk-TreeSelectionForeachFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   path  - struct TreePath, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define tree-selection-func
  (gi-lookup-callback-info "Gtk-TreeSelectionFunc"))
;; ARGS: 
;;   selection  - object TreeSelection, 
;;   model  - Unhandled argument type tag 16, 
;;   path  - struct TreePath, 
;;   path-currently-selected  - boolean, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define <GtkTreeStore>
  (gi-lookup-type "Gtk-TreeStore"))

(define (GtkTreeStore-new n-columns types)
"  ARGS: 
     n-columns  - exact integer of size gint32, 
     types  - Unhandled argument type tag 15
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeStore-new" n-columns types))

(define (GtkTreeStore-append self out-iter parent)
"  ARGS: 
     parent  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "append" out-iter parent)))

(define (GtkTreeStore-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkTreeStore-insert self out-iter parent position)
"  ARGS: 
     parent  - struct TreeIter, 
     position  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert" out-iter parent position)))

(define (GtkTreeStore-insert-after self out-iter parent sibling)
"  ARGS: 
     parent  - struct TreeIter, 
     sibling  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_after" out-iter parent sibling)))

(define (GtkTreeStore-insert-before self out-iter parent sibling)
"  ARGS: 
     parent  - struct TreeIter, 
     sibling  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_before" out-iter parent sibling)))

(define (GtkTreeStore-insert-with-values self out-iter parent position columns values n-values)
"  ARGS: 
     parent  - struct TreeIter, 
     position  - exact integer of size gint32, 
     columns  - Unhandled argument type tag 15, 
     values  - Unhandled argument type tag 15, 
     n-values  - exact integer of size gint32
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "insert_with_values" out-iter parent position columns values n-values)))

(define (GtkTreeStore-is-ancestor? self iter descendant)
"  ARGS: 
     iter  - struct TreeIter, 
     descendant  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_ancestor" iter descendant)))

(define (GtkTreeStore-iter-depth self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "iter_depth" iter)))

(define (GtkTreeStore-iter-is-valid? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_is_valid" iter)))

(define (GtkTreeStore-move-after self iter position)
"  ARGS: 
     iter  - struct TreeIter, 
     position  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_after" iter position)))

(define (GtkTreeStore-move-before self iter position)
"  ARGS: 
     iter  - struct TreeIter, 
     position  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_before" iter position)))

(define (GtkTreeStore-prepend self out-iter parent)
"  ARGS: 
     parent  - struct TreeIter
   RETURN: void
     iter  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "prepend" out-iter parent)))

(define (GtkTreeStore-remove? self iter)
"  ARGS: 
     iter  - struct TreeIter
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "remove" iter)))

(define (GtkTreeStore-set-column-types self n-columns types)
"  ARGS: 
     n-columns  - exact integer of size gint32, 
     types  - Unhandled argument type tag 15
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_types" n-columns types)))

(define (GtkTreeStore-set-value self iter column value)
"  ARGS: 
     iter  - struct TreeIter, 
     column  - exact integer of size gint32, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_value" iter column value)))

(define (GtkTreeStore-set self iter columns values n-values)
"  ARGS: 
     iter  - struct TreeIter, 
     columns  - Unhandled argument type tag 15, 
     values  - Unhandled argument type tag 15, 
     n-values  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set" iter columns values n-values)))

(define (GtkTreeStore-swap self a b)
"  ARGS: 
     a  - struct TreeIter, 
     b  - struct TreeIter
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "swap" a b)))

(define <GtkTreeView>
  (gi-lookup-type "Gtk-TreeView"))

(define (GtkTreeView-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeView-new"))

(define (GtkTreeView-new-with-model model)
"  ARGS: 
     model  - Unhandled argument type tag 16
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeView-new_with_model" model))

(define (GtkTreeView-append-column self column)
"  ARGS: 
     column  - object TreeViewColumn
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_column" column)))

(define (GtkTreeView-collapse-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "collapse_all")))

(define (GtkTreeView-collapse-row? self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "collapse_row" path)))

(define (GtkTreeView-columns-autosize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "columns_autosize")))

(define (GtkTreeView-convert-bin-window-to-tree-coords self bx by)
"  ARGS: 
     bx  - exact integer of size gint32, 
     by  - exact integer of size gint32, 
     tx  - exact integer of size gint32[OUT], 
     ty  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_bin_window_to_tree_coords" bx by)))

(define (GtkTreeView-convert-bin-window-to-widget-coords self bx by)
"  ARGS: 
     bx  - exact integer of size gint32, 
     by  - exact integer of size gint32, 
     wx  - exact integer of size gint32[OUT], 
     wy  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_bin_window_to_widget_coords" bx by)))

(define (GtkTreeView-convert-tree-to-bin-window-coords self tx ty)
"  ARGS: 
     tx  - exact integer of size gint32, 
     ty  - exact integer of size gint32, 
     bx  - exact integer of size gint32[OUT], 
     by  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_tree_to_bin_window_coords" tx ty)))

(define (GtkTreeView-convert-tree-to-widget-coords self tx ty)
"  ARGS: 
     tx  - exact integer of size gint32, 
     ty  - exact integer of size gint32, 
     wx  - exact integer of size gint32[OUT], 
     wy  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_tree_to_widget_coords" tx ty)))

(define (GtkTreeView-convert-widget-to-bin-window-coords self wx wy)
"  ARGS: 
     wx  - exact integer of size gint32, 
     wy  - exact integer of size gint32, 
     bx  - exact integer of size gint32[OUT], 
     by  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_widget_to_bin_window_coords" wx wy)))

(define (GtkTreeView-convert-widget-to-tree-coords self wx wy)
"  ARGS: 
     wx  - exact integer of size gint32, 
     wy  - exact integer of size gint32, 
     tx  - exact integer of size gint32[OUT], 
     ty  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "convert_widget_to_tree_coords" wx wy)))

(define (GtkTreeView-create-row-drag-icon self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_row_drag_icon" path)))

(define (GtkTreeView-enable-model-drag-dest self targets n-targets actions)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "enable_model_drag_dest" targets n-targets actions)))

(define (GtkTreeView-enable-model-drag-source self start-button-mask targets n-targets actions)
"  ARGS: 
     start-button-mask  - exact integer of flags type ModifierType, 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "enable_model_drag_source" start-button-mask targets n-targets actions)))

(define (GtkTreeView-expand-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "expand_all")))

(define (GtkTreeView-expand-row? self path open-all)
"  ARGS: 
     path  - struct TreePath, 
     open-all  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "expand_row" path open-all)))

(define (GtkTreeView-expand-to-path self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "expand_to_path" path)))

(define (GtkTreeView-get-activate-on-single-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_activate_on_single_click")))

(define (GtkTreeView-get-background-area self path column out-rect)
"  ARGS: 
     path  - struct TreePath, 
     column  - object TreeViewColumn, 
   RETURN: void
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_background_area" path column out-rect)))

(define (GtkTreeView-get-bin-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_bin_window")))

(define (GtkTreeView-get-cell-area self path column out-rect)
"  ARGS: 
     path  - struct TreePath, 
     column  - object TreeViewColumn, 
   RETURN: void
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_cell_area" path column out-rect)))

(define (GtkTreeView-get-column self n)
"  ARGS: 
     n  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_column" n)))

(define (GtkTreeView-get-columns self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_columns")))

(define (GtkTreeView-get-cursor self)
"  ARGS: 
     path  - struct TreePath[OUT], 
     focus-column  - object TreeViewColumn[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_cursor")))

(define (GtkTreeView-get-dest-row-at-pos? self drag-x drag-y)
"  ARGS: 
     drag-x  - exact integer of size gint32, 
     drag-y  - exact integer of size gint32, 
     path  - struct TreePath[OUT], 
     pos  - exact integer of enum type TreeViewDropPosition[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_dest_row_at_pos" drag-x drag-y)))

(define (GtkTreeView-get-drag-dest-row self)
"  ARGS: 
     path  - struct TreePath[OUT], 
     pos  - exact integer of enum type TreeViewDropPosition[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_drag_dest_row")))

(define (GtkTreeView-get-enable-search? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_enable_search")))

(define (GtkTreeView-get-enable-tree-lines? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_enable_tree_lines")))

(define (GtkTreeView-get-expander-column self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_expander_column")))

(define (GtkTreeView-get-fixed-height-mode? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_fixed_height_mode")))

(define (GtkTreeView-get-grid-lines self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_grid_lines")))

(define (GtkTreeView-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkTreeView-get-headers-clickable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_headers_clickable")))

(define (GtkTreeView-get-headers-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_headers_visible")))

(define (GtkTreeView-get-hover-expand? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hover_expand")))

(define (GtkTreeView-get-hover-selection? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hover_selection")))

(define (GtkTreeView-get-level-indentation self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_level_indentation")))

(define (GtkTreeView-get-model self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_model")))

(define (GtkTreeView-get-n-columns self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "get_n_columns")))

(define (GtkTreeView-get-path-at-pos? self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
     path  - struct TreePath[OUT], 
     column  - object TreeViewColumn[OUT], 
     cell-x  - exact integer of size gint32[OUT], 
     cell-y  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_path_at_pos" x y)))

(define (GtkTreeView-get-reorderable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reorderable")))

(define (GtkTreeView-get-rubber-banding? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_rubber_banding")))

(define (GtkTreeView-get-rules-hint? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_rules_hint")))

(define (GtkTreeView-get-search-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_search_column")))

(define (GtkTreeView-get-search-entry self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_search_entry")))

(define (GtkTreeView-get-selection self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_selection")))

(define (GtkTreeView-get-show-expanders? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_show_expanders")))

(define (GtkTreeView-get-tooltip-column self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_column")))

(define (GtkTreeView-get-tooltip-context? self x y keyboard-tip out-iter)
"  ARGS: 
     x  - exact integer of size gint32[INOUT] , 
     y  - exact integer of size gint32[INOUT] , 
     keyboard-tip  - boolean, 
     model  - #f for NULL or Unhandled argument type tag 16[OUT], 
     path  - struct TreePath[OUT], 
   RETURN: gboolean
     iter  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_context" x y keyboard-tip out-iter)))

(define (GtkTreeView-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkTreeView-get-visible-range? self)
"  ARGS: 
     start-path  - struct TreePath[OUT], 
     end-path  - struct TreePath[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_range")))

(define (GtkTreeView-get-visible-rect self out-visible-rect)
"  ARGS: 
   RETURN: void
     visible-rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_visible_rect" out-visible-rect)))

(define (GtkTreeView-insert-column self column position)
"  ARGS: 
     column  - object TreeViewColumn, 
     position  - exact integer of size gint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_column" column position)))

(define (GtkTreeView-insert-column-with-data-func self position title cell func data dnotify)
"  ARGS: 
     position  - exact integer of size gint32, 
     title  - string, 
     cell  - object CellRenderer, 
     func  - procedure of type TreeCellDataFunc, 
     data  - #f for NULL or pointer, 
     dnotify  - procedure of type DestroyNotify
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "insert_column_with_data_func" position title cell func data dnotify)))

(define (GtkTreeView-is-blank-at-pos? self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
     path  - struct TreePath[OUT], 
     column  - object TreeViewColumn[OUT], 
     cell-x  - exact integer of size gint32[OUT], 
     cell-y  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_blank_at_pos" x y)))

(define (GtkTreeView-is-rubber-banding-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_rubber_banding_active")))

(define (GtkTreeView-map-expanded-rows self func data)
"  ARGS: 
     func  - procedure of type TreeViewMappingFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "map_expanded_rows" func data)))

(define (GtkTreeView-move-column-after self column base-column)
"  ARGS: 
     column  - object TreeViewColumn, 
     base-column  - object TreeViewColumn
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move_column_after" column base-column)))

(define (GtkTreeView-remove-column self column)
"  ARGS: 
     column  - object TreeViewColumn
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "remove_column" column)))

(define (GtkTreeView-row-activated self path column)
"  ARGS: 
     path  - struct TreePath, 
     column  - object TreeViewColumn
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "row_activated" path column)))

(define (GtkTreeView-row-expanded? self path)
"  ARGS: 
     path  - struct TreePath
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "row_expanded" path)))

(define (GtkTreeView-scroll-to-cell self path column use-align row-align col-align)
"  ARGS: 
     path  - struct TreePath, 
     column  - object TreeViewColumn, 
     use-align  - boolean, 
     row-align  - real number of size gfloat, 
     col-align  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_to_cell" path column use-align row-align col-align)))

(define (GtkTreeView-scroll-to-point self tree-x tree-y)
"  ARGS: 
     tree-x  - exact integer of size gint32, 
     tree-y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "scroll_to_point" tree-x tree-y)))

(define (GtkTreeView-set-activate-on-single-click self single)
"  ARGS: 
     single  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_activate_on_single_click" single)))

(define (GtkTreeView-set-column-drag-function self func user-data destroy)
"  ARGS: 
     func  - procedure of type TreeViewColumnDropFunc, 
     user-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_column_drag_function" func user-data destroy)))

(define (GtkTreeView-set-cursor self path focus-column start-editing)
"  ARGS: 
     path  - struct TreePath, 
     focus-column  - object TreeViewColumn, 
     start-editing  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor" path focus-column start-editing)))

(define (GtkTreeView-set-cursor-on-cell self path focus-column focus-cell start-editing)
"  ARGS: 
     path  - struct TreePath, 
     focus-column  - object TreeViewColumn, 
     focus-cell  - object CellRenderer, 
     start-editing  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cursor_on_cell" path focus-column focus-cell start-editing)))

(define (GtkTreeView-set-destroy-count-func self func data destroy)
"  ARGS: 
     func  - procedure of type TreeDestroyCountFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_destroy_count_func" func data destroy)))

(define (GtkTreeView-set-drag-dest-row self path pos)
"  ARGS: 
     path  - struct TreePath, 
     pos  - exact integer of enum type TreeViewDropPosition
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_drag_dest_row" path pos)))

(define (GtkTreeView-set-enable-search self enable-search)
"  ARGS: 
     enable-search  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_enable_search" enable-search)))

(define (GtkTreeView-set-enable-tree-lines self enabled)
"  ARGS: 
     enabled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_enable_tree_lines" enabled)))

(define (GtkTreeView-set-expander-column self column)
"  ARGS: 
     column  - object TreeViewColumn
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_expander_column" column)))

(define (GtkTreeView-set-fixed-height-mode self enable)
"  ARGS: 
     enable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fixed_height_mode" enable)))

(define (GtkTreeView-set-grid-lines self grid-lines)
"  ARGS: 
     grid-lines  - exact integer of enum type TreeViewGridLines
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_grid_lines" grid-lines)))

(define (GtkTreeView-set-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hadjustment" adjustment)))

(define (GtkTreeView-set-headers-clickable self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_headers_clickable" setting)))

(define (GtkTreeView-set-headers-visible self headers-visible)
"  ARGS: 
     headers-visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_headers_visible" headers-visible)))

(define (GtkTreeView-set-hover-expand self expand)
"  ARGS: 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hover_expand" expand)))

(define (GtkTreeView-set-hover-selection self hover)
"  ARGS: 
     hover  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hover_selection" hover)))

(define (GtkTreeView-set-level-indentation self indentation)
"  ARGS: 
     indentation  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_level_indentation" indentation)))

(define (GtkTreeView-set-model self model)
"  ARGS: 
     model  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_model" model)))

(define (GtkTreeView-set-reorderable self reorderable)
"  ARGS: 
     reorderable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reorderable" reorderable)))

(define (GtkTreeView-set-row-separator-func self func data destroy)
"  ARGS: 
     func  - procedure of type TreeViewRowSeparatorFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_row_separator_func" func data destroy)))

(define (GtkTreeView-set-rubber-banding self enable)
"  ARGS: 
     enable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_rubber_banding" enable)))

(define (GtkTreeView-set-rules-hint self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_rules_hint" setting)))

(define (GtkTreeView-set-search-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_column" column)))

(define (GtkTreeView-set-search-entry self entry)
"  ARGS: 
     entry  - object Entry
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_entry" entry)))

(define (GtkTreeView-set-search-equal-func self search-equal-func search-user-data search-destroy)
"  ARGS: 
     search-equal-func  - procedure of type TreeViewSearchEqualFunc, 
     search-user-data  - #f for NULL or pointer, 
     search-destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_equal_func" search-equal-func search-user-data search-destroy)))

(define (GtkTreeView-set-search-position-func self func data destroy)
"  ARGS: 
     func  - procedure of type TreeViewSearchPositionFunc, 
     data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_search_position_func" func data destroy)))

(define (GtkTreeView-set-show-expanders self enabled)
"  ARGS: 
     enabled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_show_expanders" enabled)))

(define (GtkTreeView-set-tooltip-cell self tooltip path column cell)
"  ARGS: 
     tooltip  - object Tooltip, 
     path  - struct TreePath, 
     column  - object TreeViewColumn, 
     cell  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_cell" tooltip path column cell)))

(define (GtkTreeView-set-tooltip-column self column)
"  ARGS: 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_column" column)))

(define (GtkTreeView-set-tooltip-row self tooltip path)
"  ARGS: 
     tooltip  - object Tooltip, 
     path  - struct TreePath
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_row" tooltip path)))

(define (GtkTreeView-set-vadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vadjustment" adjustment)))

(define (GtkTreeView-unset-rows-drag-dest self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_rows_drag_dest")))

(define (GtkTreeView-unset-rows-drag-source self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_rows_drag_source")))

(define <GtkTreeViewAccessible>
  (gi-lookup-type "Gtk-TreeViewAccessible"))

(define <GtkTreeViewColumn>
  (gi-lookup-type "Gtk-TreeViewColumn"))

(define (GtkTreeViewColumn-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeViewColumn-new"))

(define (GtkTreeViewColumn-new-with-area area)
"  ARGS: 
     area  - object CellArea
   RETURN: interface*
"
  (gi-function-invoke "Gtk-TreeViewColumn-new_with_area" area))

(define (GtkTreeViewColumn-add-attribute self cell-renderer attribute column)
"  ARGS: 
     cell-renderer  - object CellRenderer, 
     attribute  - string, 
     column  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_attribute" cell-renderer attribute column)))

(define (GtkTreeViewColumn-cell-get-position? self cell-renderer)
"  ARGS: 
     cell-renderer  - object CellRenderer, 
     x-offset  - exact integer of size gint32[OUT], 
     width  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "cell_get_position" cell-renderer)))

(define (GtkTreeViewColumn-cell-get-size self cell-area)
"  ARGS: 
     cell-area  - struct Rectangle, 
     x-offset  - exact integer of size gint32[OUT], 
     y-offset  - exact integer of size gint32[OUT], 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cell_get_size" cell-area)))

(define (GtkTreeViewColumn-cell-is-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "cell_is_visible")))

(define (GtkTreeViewColumn-cell-set-cell-data self tree-model iter is-expander is-expanded)
"  ARGS: 
     tree-model  - Unhandled argument type tag 16, 
     iter  - struct TreeIter, 
     is-expander  - boolean, 
     is-expanded  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "cell_set_cell_data" tree-model iter is-expander is-expanded)))

(define (GtkTreeViewColumn-clear self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear")))

(define (GtkTreeViewColumn-clear-attributes self cell-renderer)
"  ARGS: 
     cell-renderer  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clear_attributes" cell-renderer)))

(define (GtkTreeViewColumn-clicked self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "clicked")))

(define (GtkTreeViewColumn-focus-cell self cell)
"  ARGS: 
     cell  - object CellRenderer
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "focus_cell" cell)))

(define (GtkTreeViewColumn-get-alignment self)
"  ARGS: 
   RETURN: gfloat
"
  (gi-method-send self 
     (gi-method-prepare "get_alignment")))

(define (GtkTreeViewColumn-get-button self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_button")))

(define (GtkTreeViewColumn-get-clickable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_clickable")))

(define (GtkTreeViewColumn-get-expand? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_expand")))

(define (GtkTreeViewColumn-get-fixed-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_fixed_width")))

(define (GtkTreeViewColumn-get-max-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_max_width")))

(define (GtkTreeViewColumn-get-min-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_min_width")))

(define (GtkTreeViewColumn-get-reorderable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_reorderable")))

(define (GtkTreeViewColumn-get-resizable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_resizable")))

(define (GtkTreeViewColumn-get-sizing self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_sizing")))

(define (GtkTreeViewColumn-get-sort-column-id self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_sort_column_id")))

(define (GtkTreeViewColumn-get-sort-indicator? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_sort_indicator")))

(define (GtkTreeViewColumn-get-sort-order self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_sort_order")))

(define (GtkTreeViewColumn-get-spacing self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_spacing")))

(define (GtkTreeViewColumn-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkTreeViewColumn-get-tree-view self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tree_view")))

(define (GtkTreeViewColumn-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkTreeViewColumn-get-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget")))

(define (GtkTreeViewColumn-get-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_width")))

(define (GtkTreeViewColumn-get-x-offset self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_x_offset")))

(define (GtkTreeViewColumn-pack-end self cell expand)
"  ARGS: 
     cell  - object CellRenderer, 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_end" cell expand)))

(define (GtkTreeViewColumn-pack-start self cell expand)
"  ARGS: 
     cell  - object CellRenderer, 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "pack_start" cell expand)))

(define (GtkTreeViewColumn-queue-resize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_resize")))

(define (GtkTreeViewColumn-set-alignment self xalign)
"  ARGS: 
     xalign  - real number of size gfloat
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_alignment" xalign)))

(define (GtkTreeViewColumn-set-cell-data-func self cell-renderer func func-data destroy)
"  ARGS: 
     cell-renderer  - object CellRenderer, 
     func  - procedure of type TreeCellDataFunc, 
     func-data  - #f for NULL or pointer, 
     destroy  - procedure of type DestroyNotify
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_cell_data_func" cell-renderer func func-data destroy)))

(define (GtkTreeViewColumn-set-clickable self clickable)
"  ARGS: 
     clickable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_clickable" clickable)))

(define (GtkTreeViewColumn-set-expand self expand)
"  ARGS: 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_expand" expand)))

(define (GtkTreeViewColumn-set-fixed-width self fixed-width)
"  ARGS: 
     fixed-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_fixed_width" fixed-width)))

(define (GtkTreeViewColumn-set-max-width self max-width)
"  ARGS: 
     max-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_max_width" max-width)))

(define (GtkTreeViewColumn-set-min-width self min-width)
"  ARGS: 
     min-width  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_min_width" min-width)))

(define (GtkTreeViewColumn-set-reorderable self reorderable)
"  ARGS: 
     reorderable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_reorderable" reorderable)))

(define (GtkTreeViewColumn-set-resizable self resizable)
"  ARGS: 
     resizable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resizable" resizable)))

(define (GtkTreeViewColumn-set-sizing self type)
"  ARGS: 
     type  - exact integer of enum type TreeViewColumnSizing
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sizing" type)))

(define (GtkTreeViewColumn-set-sort-column-id self sort-column-id)
"  ARGS: 
     sort-column-id  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sort_column_id" sort-column-id)))

(define (GtkTreeViewColumn-set-sort-indicator self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sort_indicator" setting)))

(define (GtkTreeViewColumn-set-sort-order self order)
"  ARGS: 
     order  - exact integer of enum type SortType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sort_order" order)))

(define (GtkTreeViewColumn-set-spacing self spacing)
"  ARGS: 
     spacing  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_spacing" spacing)))

(define (GtkTreeViewColumn-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkTreeViewColumn-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define (GtkTreeViewColumn-set-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_widget" widget)))

;; CALLBACK
(define tree-view-column-drop-func
  (gi-lookup-callback-info "Gtk-TreeViewColumnDropFunc"))
;; ARGS: 
;;   tree-view  - object TreeView, 
;;   column  - object TreeViewColumn, 
;;   prev-column  - object TreeViewColumn, 
;;   next-column  - object TreeViewColumn, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
(define TREE_VIEW_COLUMN_SIZING_GROW_ONLY
  (gi-enum-value "Gtk-TreeViewColumnSizing" "grow_only"))

(define TREE_VIEW_COLUMN_SIZING_AUTOSIZE
  (gi-enum-value "Gtk-TreeViewColumnSizing" "autosize"))

(define TREE_VIEW_COLUMN_SIZING_FIXED
  (gi-enum-value "Gtk-TreeViewColumnSizing" "fixed"))

(define TREE_VIEW_DROP_POSITION_BEFORE
  (gi-enum-value "Gtk-TreeViewDropPosition" "before"))

(define TREE_VIEW_DROP_POSITION_AFTER
  (gi-enum-value "Gtk-TreeViewDropPosition" "after"))

(define TREE_VIEW_DROP_POSITION_INTO_OR_BEFORE
  (gi-enum-value "Gtk-TreeViewDropPosition" "into_or_before"))

(define TREE_VIEW_DROP_POSITION_INTO_OR_AFTER
  (gi-enum-value "Gtk-TreeViewDropPosition" "into_or_after"))

(define TREE_VIEW_GRID_LINES_NONE
  (gi-enum-value "Gtk-TreeViewGridLines" "none"))

(define TREE_VIEW_GRID_LINES_HORIZONTAL
  (gi-enum-value "Gtk-TreeViewGridLines" "horizontal"))

(define TREE_VIEW_GRID_LINES_VERTICAL
  (gi-enum-value "Gtk-TreeViewGridLines" "vertical"))

(define TREE_VIEW_GRID_LINES_BOTH
  (gi-enum-value "Gtk-TreeViewGridLines" "both"))

;; CALLBACK
(define tree-view-mapping-func
  (gi-lookup-callback-info "Gtk-TreeViewMappingFunc"))
;; ARGS: 
;;   tree-view  - object TreeView, 
;;   path  - struct TreePath, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
;; CALLBACK
(define tree-view-row-separator-func
  (gi-lookup-callback-info "Gtk-TreeViewRowSeparatorFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   iter  - struct TreeIter, 
;;   data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define tree-view-search-equal-func
  (gi-lookup-callback-info "Gtk-TreeViewSearchEqualFunc"))
;; ARGS: 
;;   model  - Unhandled argument type tag 16, 
;;   column  - exact integer of size gint32, 
;;   key  - string, 
;;   iter  - struct TreeIter, 
;;   search-data  - #f for NULL or pointer
;; RETURN: gboolean
;; CALLBACK
(define tree-view-search-position-func
  (gi-lookup-callback-info "Gtk-TreeViewSearchPositionFunc"))
;; ARGS: 
;;   tree-view  - object TreeView, 
;;   search-dialog  - object Widget, 
;;   user-data  - #f for NULL or pointer
;; RETURN: void
(define <GtkUIManager>
  (gi-lookup-type "Gtk-UIManager"))

(define (GtkUIManager-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-UIManager-new"))

(define (GtkUIManager-add-ui self merge-id path name action type top)
"  ARGS: 
     merge-id  - exact integer of size guint32, 
     path  - string, 
     name  - string, 
     action  - #f for NULL or string, 
     type  - exact integer of flags type UIManagerItemType, 
     top  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_ui" merge-id path name action type top)))

(define (GtkUIManager-add-ui-from-file self filename)
"  ARGS: 
     filename  - locale string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_ui_from_file" filename)))

(define (GtkUIManager-add-ui-from-resource self resource-path)
"  ARGS: 
     resource-path  - string
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_ui_from_resource" resource-path)))

(define (GtkUIManager-add-ui-from-string self buffer length)
"  ARGS: 
     buffer  - string, 
     length  - exact integer of size gint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_ui_from_string" buffer length)))

(define (GtkUIManager-ensure-update self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "ensure_update")))

(define (GtkUIManager-get-accel-group self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accel_group")))

(define (GtkUIManager-get-action self path)
"  ARGS: 
     path  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action" path)))

(define (GtkUIManager-get-action-groups self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_action_groups")))

(define (GtkUIManager-get-add-tearoffs? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_add_tearoffs")))

(define (GtkUIManager-get-toplevels self types)
"  ARGS: 
     types  - exact integer of flags type UIManagerItemType
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "get_toplevels" types)))

(define (GtkUIManager-get-ui self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_ui")))

(define (GtkUIManager-get-widget self path)
"  ARGS: 
     path  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_widget" path)))

(define (GtkUIManager-insert-action-group self action-group pos)
"  ARGS: 
     action-group  - object ActionGroup, 
     pos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_action_group" action-group pos)))

(define (GtkUIManager-new-merge-id self)
"  ARGS: 
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "new_merge_id")))

(define (GtkUIManager-remove-action-group self action-group)
"  ARGS: 
     action-group  - object ActionGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_action_group" action-group)))

(define (GtkUIManager-remove-ui self merge-id)
"  ARGS: 
     merge-id  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_ui" merge-id)))

(define (GtkUIManager-set-add-tearoffs self add-tearoffs)
"  ARGS: 
     add-tearoffs  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_add_tearoffs" add-tearoffs)))

(define UNIT_NONE
  (gi-enum-value "Gtk-Unit" "none"))

(define UNIT_POINTS
  (gi-enum-value "Gtk-Unit" "points"))

(define UNIT_INCH
  (gi-enum-value "Gtk-Unit" "inch"))

(define UNIT_MM
  (gi-enum-value "Gtk-Unit" "mm"))

(define <GtkVBox>
  (gi-lookup-type "Gtk-VBox"))

(define (GtkVBox-new homogeneous spacing)
"  ARGS: 
     homogeneous  - boolean, 
     spacing  - exact integer of size gint32
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VBox-new" homogeneous spacing))

(define <GtkVButtonBox>
  (gi-lookup-type "Gtk-VButtonBox"))

(define (GtkVButtonBox-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VButtonBox-new"))

(define <GtkVPaned>
  (gi-lookup-type "Gtk-VPaned"))

(define (GtkVPaned-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VPaned-new"))

(define <GtkVScale>
  (gi-lookup-type "Gtk-VScale"))

(define (GtkVScale-new adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VScale-new" adjustment))

(define (GtkVScale-new-with-range min max step)
"  ARGS: 
     min  - real number of size gdouble, 
     max  - real number of size gdouble, 
     step  - real number of size gdouble
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VScale-new_with_range" min max step))

(define <GtkVScrollbar>
  (gi-lookup-type "Gtk-VScrollbar"))

(define (GtkVScrollbar-new adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VScrollbar-new" adjustment))

(define <GtkVSeparator>
  (gi-lookup-type "Gtk-VSeparator"))

(define (GtkVSeparator-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VSeparator-new"))

(define <GtkViewport>
  (gi-lookup-type "Gtk-Viewport"))

(define (GtkViewport-new hadjustment vadjustment)
"  ARGS: 
     hadjustment  - object Adjustment, 
     vadjustment  - object Adjustment
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Viewport-new" hadjustment vadjustment))

(define (GtkViewport-get-bin-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_bin_window")))

(define (GtkViewport-get-hadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_hadjustment")))

(define (GtkViewport-get-shadow-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_shadow_type")))

(define (GtkViewport-get-vadjustment self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_vadjustment")))

(define (GtkViewport-get-view-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_view_window")))

(define (GtkViewport-set-hadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hadjustment" adjustment)))

(define (GtkViewport-set-shadow-type self type)
"  ARGS: 
     type  - exact integer of enum type ShadowType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_shadow_type" type)))

(define (GtkViewport-set-vadjustment self adjustment)
"  ARGS: 
     adjustment  - object Adjustment
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vadjustment" adjustment)))

(define <GtkVolumeButton>
  (gi-lookup-type "Gtk-VolumeButton"))

(define (GtkVolumeButton-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-VolumeButton-new"))

(define <GtkWidget>
  (gi-lookup-type "Gtk-Widget"))

(define (GtkWidget-get-default-direction)
"  ARGS: 
   RETURN: interface
"
  (gi-function-invoke "Gtk-Widget-get_default_direction"))

(define (GtkWidget-get-default-style)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Widget-get_default_style"))

(define (GtkWidget-pop-composite-child)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-Widget-pop_composite_child"))

(define (GtkWidget-push-composite-child)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-Widget-push_composite_child"))

(define (GtkWidget-set-default-direction dir)
"  ARGS: 
     dir  - exact integer of enum type TextDirection
   RETURN: void
"
  (gi-function-invoke "Gtk-Widget-set_default_direction" dir))

(define (GtkWidget-activate? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate")))

(define (GtkWidget-add-accelerator self accel-signal accel-group accel-key accel-mods accel-flags)
"  ARGS: 
     accel-signal  - string, 
     accel-group  - object AccelGroup, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType, 
     accel-flags  - exact integer of flags type AccelFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_accelerator" accel-signal accel-group accel-key accel-mods accel-flags)))

(define (GtkWidget-add-device-events self device events)
"  ARGS: 
     device  - object Device, 
     events  - exact integer of flags type EventMask
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_device_events" device events)))

(define (GtkWidget-add-events self events)
"  ARGS: 
     events  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_events" events)))

(define (GtkWidget-add-mnemonic-label self label)
"  ARGS: 
     label  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mnemonic_label" label)))

(define (GtkWidget-add-tick-callback self callback user-data notify)
"  ARGS: 
     callback  - procedure of type TickCallback, 
     user-data  - #f for NULL or pointer, 
     notify  - procedure of type DestroyNotify
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "add_tick_callback" callback user-data notify)))

(define (GtkWidget-can-activate-accel? self signal-id)
"  ARGS: 
     signal-id  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "can_activate_accel" signal-id)))

(define (GtkWidget-child-focus? self direction)
"  ARGS: 
     direction  - exact integer of enum type DirectionType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "child_focus" direction)))

(define (GtkWidget-child-notify self child-property)
"  ARGS: 
     child-property  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "child_notify" child-property)))

(define (GtkWidget-class-path self)
"  ARGS: 
     path-length  - exact integer of size guint32[OUT], 
     path  - string[OUT], 
     path-reversed  - string[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "class_path")))

(define (GtkWidget-compute-expand? self orientation)
"  ARGS: 
     orientation  - exact integer of enum type Orientation
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "compute_expand" orientation)))

(define (GtkWidget-create-pango-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_pango_context")))

(define (GtkWidget-create-pango-layout self text)
"  ARGS: 
     text  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "create_pango_layout" text)))

(define (GtkWidget-destroy self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "destroy")))

(define (GtkWidget-destroyed self widget-pointer)
"  ARGS: 
     widget-pointer  - object Widget[INOUT] 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "destroyed" widget-pointer)))

(define (GtkWidget-device-is-shadowed? self device)
"  ARGS: 
     device  - object Device
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "device_is_shadowed" device)))

(define (GtkWidget-drag-begin self targets actions button event)
"  ARGS: 
     targets  - struct TargetList, 
     actions  - exact integer of flags type DragAction, 
     button  - exact integer of size gint32, 
     event  - union Event
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "drag_begin" targets actions button event)))

(define (GtkWidget-drag-begin-with-coordinates self targets actions button event x y)
"  ARGS: 
     targets  - struct TargetList, 
     actions  - exact integer of flags type DragAction, 
     button  - exact integer of size gint32, 
     event  - union Event, 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "drag_begin_with_coordinates" targets actions button event x y)))

(define (GtkWidget-drag-check-threshold? self start-x start-y current-x current-y)
"  ARGS: 
     start-x  - exact integer of size gint32, 
     start-y  - exact integer of size gint32, 
     current-x  - exact integer of size gint32, 
     current-y  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "drag_check_threshold" start-x start-y current-x current-y)))

(define (GtkWidget-drag-dest-add-image-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_add_image_targets")))

(define (GtkWidget-drag-dest-add-text-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_add_text_targets")))

(define (GtkWidget-drag-dest-add-uri-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_add_uri_targets")))

(define (GtkWidget-drag-dest-find-target self context target-list)
"  ARGS: 
     context  - object DragContext, 
     target-list  - struct TargetList
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_find_target" context target-list)))

(define (GtkWidget-drag-dest-get-target-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_get_target_list")))

(define (GtkWidget-drag-dest-get-track-motion? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_get_track_motion")))

(define (GtkWidget-drag-dest-set self flags targets n-targets actions)
"  ARGS: 
     flags  - exact integer of flags type DestDefaults, 
     targets  - #f for NULL or Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_set" flags targets n-targets actions)))

(define (GtkWidget-drag-dest-set-proxy self proxy-window protocol use-coordinates)
"  ARGS: 
     proxy-window  - object Window, 
     protocol  - exact integer of enum type DragProtocol, 
     use-coordinates  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_set_proxy" proxy-window protocol use-coordinates)))

(define (GtkWidget-drag-dest-set-target-list self target-list)
"  ARGS: 
     target-list  - struct TargetList
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_set_target_list" target-list)))

(define (GtkWidget-drag-dest-set-track-motion self track-motion)
"  ARGS: 
     track-motion  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_set_track_motion" track-motion)))

(define (GtkWidget-drag-dest-unset self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_dest_unset")))

(define (GtkWidget-drag-get-data self context target time-)
"  ARGS: 
     context  - object DragContext, 
     target  - struct Atom, 
     time-  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_get_data" context target time-)))

(define (GtkWidget-drag-highlight self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_highlight")))

(define (GtkWidget-drag-source-add-image-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_add_image_targets")))

(define (GtkWidget-drag-source-add-text-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_add_text_targets")))

(define (GtkWidget-drag-source-add-uri-targets self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_add_uri_targets")))

(define (GtkWidget-drag-source-get-target-list self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_get_target_list")))

(define (GtkWidget-drag-source-set self start-button-mask targets n-targets actions)
"  ARGS: 
     start-button-mask  - exact integer of flags type ModifierType, 
     targets  - #f for NULL or Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     actions  - exact integer of flags type DragAction
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set" start-button-mask targets n-targets actions)))

(define (GtkWidget-drag-source-set-icon-gicon self icon)
"  ARGS: 
     icon  - Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set_icon_gicon" icon)))

(define (GtkWidget-drag-source-set-icon-name self icon-name)
"  ARGS: 
     icon-name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set_icon_name" icon-name)))

(define (GtkWidget-drag-source-set-icon-pixbuf self pixbuf)
"  ARGS: 
     pixbuf  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set_icon_pixbuf" pixbuf)))

(define (GtkWidget-drag-source-set-icon-stock self stock-id)
"  ARGS: 
     stock-id  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set_icon_stock" stock-id)))

(define (GtkWidget-drag-source-set-target-list self target-list)
"  ARGS: 
     target-list  - struct TargetList
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_set_target_list" target-list)))

(define (GtkWidget-drag-source-unset self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_source_unset")))

(define (GtkWidget-drag-unhighlight self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "drag_unhighlight")))

(define (GtkWidget-draw self cr)
"  ARGS: 
     cr  - struct Context
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "draw" cr)))

(define (GtkWidget-ensure-style self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "ensure_style")))

(define (GtkWidget-error-bell self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "error_bell")))

(define (GtkWidget-event? self event)
"  ARGS: 
     event  - union Event
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "event" event)))

(define (GtkWidget-freeze-child-notify self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "freeze_child_notify")))

(define (GtkWidget-get-accessible self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_accessible")))

(define (GtkWidget-get-action-group self prefix)
"  ARGS: 
     prefix  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_action_group" prefix)))

(define (GtkWidget-get-allocated-baseline self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_allocated_baseline")))

(define (GtkWidget-get-allocated-height self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_allocated_height")))

(define (GtkWidget-get-allocated-size self out-allocation)
"  ARGS: 
     baseline  - exact integer of size gint32[OUT]
   RETURN: void
     allocation  - Unhandled argument type tag 16, 
"
  (gi-method-send self 
     (gi-method-prepare "get_allocated_size" out-allocation)))

(define (GtkWidget-get-allocated-width self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_allocated_width")))

(define (GtkWidget-get-allocation self out-allocation)
"  ARGS: 
   RETURN: void
     allocation  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_allocation" out-allocation)))

(define (GtkWidget-get-ancestor self widget-type)
"  ARGS: 
     widget-type  - <GType>
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_ancestor" widget-type)))

(define (GtkWidget-get-app-paintable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_app_paintable")))

(define (GtkWidget-get-can-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_can_default")))

(define (GtkWidget-get-can-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_can_focus")))

(define (GtkWidget-get-child-requisition self out-requisition)
"  ARGS: 
   RETURN: void
     requisition  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_child_requisition" out-requisition)))

(define (GtkWidget-get-child-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_child_visible")))

(define (GtkWidget-get-clip self out-clip)
"  ARGS: 
   RETURN: void
     clip  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_clip" out-clip)))

(define (GtkWidget-get-clipboard self selection)
"  ARGS: 
     selection  - struct Atom
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_clipboard" selection)))

(define (GtkWidget-get-composite-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_composite_name")))

(define (GtkWidget-get-device-enabled? self device)
"  ARGS: 
     device  - object Device
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_device_enabled" device)))

(define (GtkWidget-get-device-events self device)
"  ARGS: 
     device  - object Device
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_device_events" device)))

(define (GtkWidget-get-direction self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_direction")))

(define (GtkWidget-get-display self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_display")))

(define (GtkWidget-get-double-buffered? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_double_buffered")))

(define (GtkWidget-get-events self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_events")))

(define (GtkWidget-get-focus-on-click? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_on_click")))

(define (GtkWidget-get-font-map self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_map")))

(define (GtkWidget-get-font-options self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_font_options")))

(define (GtkWidget-get-frame-clock self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_frame_clock")))

(define (GtkWidget-get-halign self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_halign")))

(define (GtkWidget-get-has-tooltip? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_tooltip")))

(define (GtkWidget-get-has-window? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_window")))

(define (GtkWidget-get-hexpand? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hexpand")))

(define (GtkWidget-get-hexpand-set? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hexpand_set")))

(define (GtkWidget-get-mapped? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_mapped")))

(define (GtkWidget-get-margin-bottom self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_bottom")))

(define (GtkWidget-get-margin-end self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_end")))

(define (GtkWidget-get-margin-left self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_left")))

(define (GtkWidget-get-margin-right self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_right")))

(define (GtkWidget-get-margin-start self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_start")))

(define (GtkWidget-get-margin-top self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_margin_top")))

(define (GtkWidget-get-modifier-mask self intent)
"  ARGS: 
     intent  - exact integer of enum type ModifierIntent
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_modifier_mask" intent)))

(define (GtkWidget-get-modifier-style self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_modifier_style")))

(define (GtkWidget-get-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_name")))

(define (GtkWidget-get-no-show-all? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_no_show_all")))

(define (GtkWidget-get-opacity self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_opacity")))

(define (GtkWidget-get-pango-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_pango_context")))

(define (GtkWidget-get-parent self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent")))

(define (GtkWidget-get-parent-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_parent_window")))

(define (GtkWidget-get-path self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_path")))

(define (GtkWidget-get-pointer self)
"  ARGS: 
     x  - exact integer of size gint32[OUT], 
     y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_pointer")))

(define (GtkWidget-get-preferred-height self)
"  ARGS: 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height")))

(define (GtkWidget-get-preferred-height-and-baseline-for-width self width)
"  ARGS: 
     width  - exact integer of size gint32, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT], 
     minimum-baseline  - exact integer of size gint32[OUT], 
     natural-baseline  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height_and_baseline_for_width" width)))

(define (GtkWidget-get-preferred-height-for-width self width)
"  ARGS: 
     width  - exact integer of size gint32, 
     minimum-height  - exact integer of size gint32[OUT], 
     natural-height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_height_for_width" width)))

(define (GtkWidget-get-preferred-size self out-minimum-size out-natural-size)
"  ARGS: 
   RETURN: void
     minimum-size  - Unhandled argument type tag 16, 
     natural-size  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_size" out-minimum-size out-natural-size)))

(define (GtkWidget-get-preferred-width self)
"  ARGS: 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width")))

(define (GtkWidget-get-preferred-width-for-height self height)
"  ARGS: 
     height  - exact integer of size gint32, 
     minimum-width  - exact integer of size gint32[OUT], 
     natural-width  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_preferred_width_for_height" height)))

(define (GtkWidget-get-realized? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_realized")))

(define (GtkWidget-get-receives-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_receives_default")))

(define (GtkWidget-get-request-mode self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_request_mode")))

(define (GtkWidget-get-requisition self out-requisition)
"  ARGS: 
   RETURN: void
     requisition  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_requisition" out-requisition)))

(define (GtkWidget-get-root-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_root_window")))

(define (GtkWidget-get-scale-factor self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "get_scale_factor")))

(define (GtkWidget-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkWidget-get-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_sensitive")))

(define (GtkWidget-get-settings self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_settings")))

(define (GtkWidget-get-size-request self)
"  ARGS: 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_size_request")))

(define (GtkWidget-get-state self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state")))

(define (GtkWidget-get-state-flags self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_state_flags")))

(define (GtkWidget-get-style self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_style")))

(define (GtkWidget-get-style-context self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_style_context")))

(define (GtkWidget-get-support-multidevice? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_support_multidevice")))

(define (GtkWidget-get-template-child self widget-type name)
"  ARGS: 
     widget-type  - <GType>, 
     name  - string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_template_child" widget-type name)))

(define (GtkWidget-get-tooltip-markup self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_markup")))

(define (GtkWidget-get-tooltip-text self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_text")))

(define (GtkWidget-get-tooltip-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_tooltip_window")))

(define (GtkWidget-get-toplevel self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_toplevel")))

(define (GtkWidget-get-valign self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_valign")))

(define (GtkWidget-get-valign-with-baseline self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_valign_with_baseline")))

(define (GtkWidget-get-vexpand? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_vexpand")))

(define (GtkWidget-get-vexpand-set? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_vexpand_set")))

(define (GtkWidget-get-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_visible")))

(define (GtkWidget-get-visual self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_visual")))

(define (GtkWidget-get-window self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_window")))

(define (GtkWidget-grab-add self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "grab_add")))

(define (GtkWidget-grab-default self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "grab_default")))

(define (GtkWidget-grab-focus self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "grab_focus")))

(define (GtkWidget-grab-remove self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "grab_remove")))

(define (GtkWidget-has-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_default")))

(define (GtkWidget-has-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_focus")))

(define (GtkWidget-has-grab? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_grab")))

(define (GtkWidget-has-rc-style? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_rc_style")))

(define (GtkWidget-has-screen? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_screen")))

(define (GtkWidget-has-visible-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_visible_focus")))

(define (GtkWidget-hide self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "hide")))

(define (GtkWidget-hide-on-delete? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "hide_on_delete")))

(define (GtkWidget-in-destruction? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "in_destruction")))

(define (GtkWidget-init-template self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "init_template")))

(define (GtkWidget-input-shape-combine-region self region)
"  ARGS: 
     region  - struct Region
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "input_shape_combine_region" region)))

(define (GtkWidget-insert-action-group self name group)
"  ARGS: 
     name  - string, 
     group  - #f for NULL or Unhandled argument type tag 16
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "insert_action_group" name group)))

(define (GtkWidget-intersect? self area out-intersection)
"  ARGS: 
     area  - struct Rectangle, 
   RETURN: gboolean
     intersection  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "intersect" area out-intersection)))

(define (GtkWidget-is-ancestor? self ancestor)
"  ARGS: 
     ancestor  - object Widget
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_ancestor" ancestor)))

(define (GtkWidget-is-composited? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_composited")))

(define (GtkWidget-is-drawable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_drawable")))

(define (GtkWidget-is-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_focus")))

(define (GtkWidget-is-sensitive? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_sensitive")))

(define (GtkWidget-is-toplevel? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_toplevel")))

(define (GtkWidget-is-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_visible")))

(define (GtkWidget-keynav-failed? self direction)
"  ARGS: 
     direction  - exact integer of enum type DirectionType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "keynav_failed" direction)))

(define (GtkWidget-list-accel-closures self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_accel_closures")))

(define (GtkWidget-list-action-prefixes self)
"  ARGS: 
   RETURN: array*
"
  (gi-method-send self 
     (gi-method-prepare "list_action_prefixes")))

(define (GtkWidget-list-mnemonic-labels self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_mnemonic_labels")))

(define (GtkWidget-map self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "map")))

(define (GtkWidget-mnemonic-activate? self group-cycling)
"  ARGS: 
     group-cycling  - boolean
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "mnemonic_activate" group-cycling)))

(define (GtkWidget-modify-base self state color)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_base" state color)))

(define (GtkWidget-modify-bg self state color)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_bg" state color)))

(define (GtkWidget-modify-cursor self primary secondary)
"  ARGS: 
     primary  - struct Color, 
     secondary  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_cursor" primary secondary)))

(define (GtkWidget-modify-fg self state color)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_fg" state color)))

(define (GtkWidget-modify-font self font-desc)
"  ARGS: 
     font-desc  - struct FontDescription
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_font" font-desc)))

(define (GtkWidget-modify-style self style)
"  ARGS: 
     style  - object RcStyle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_style" style)))

(define (GtkWidget-modify-text self state color)
"  ARGS: 
     state  - exact integer of enum type StateType, 
     color  - struct Color
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "modify_text" state color)))

(define (GtkWidget-override-background-color self state color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
     color  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "override_background_color" state color)))

(define (GtkWidget-override-color self state color)
"  ARGS: 
     state  - exact integer of flags type StateFlags, 
     color  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "override_color" state color)))

(define (GtkWidget-override-cursor self cursor secondary-cursor)
"  ARGS: 
     cursor  - struct RGBA, 
     secondary-cursor  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "override_cursor" cursor secondary-cursor)))

(define (GtkWidget-override-font self font-desc)
"  ARGS: 
     font-desc  - struct FontDescription
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "override_font" font-desc)))

(define (GtkWidget-override-symbolic-color self name color)
"  ARGS: 
     name  - string, 
     color  - struct RGBA
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "override_symbolic_color" name color)))

(define (GtkWidget-path self)
"  ARGS: 
     path-length  - exact integer of size guint32[OUT], 
     path  - string[OUT], 
     path-reversed  - string[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "path")))

(define (GtkWidget-queue-allocate self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_allocate")))

(define (GtkWidget-queue-compute-expand self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_compute_expand")))

(define (GtkWidget-queue-draw self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_draw")))

(define (GtkWidget-queue-draw-area self x y width height)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32, 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_draw_area" x y width height)))

(define (GtkWidget-queue-draw-region self region)
"  ARGS: 
     region  - struct Region
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_draw_region" region)))

(define (GtkWidget-queue-resize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_resize")))

(define (GtkWidget-queue-resize-no-redraw self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "queue_resize_no_redraw")))

(define (GtkWidget-realize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "realize")))

(define (GtkWidget-region-intersect self region)
"  ARGS: 
     region  - struct Region
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "region_intersect" region)))

(define (GtkWidget-register-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "register_window" window)))

(define (GtkWidget-remove-accelerator? self accel-group accel-key accel-mods)
"  ARGS: 
     accel-group  - object AccelGroup, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "remove_accelerator" accel-group accel-key accel-mods)))

(define (GtkWidget-remove-mnemonic-label self label)
"  ARGS: 
     label  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_mnemonic_label" label)))

(define (GtkWidget-remove-tick-callback self id)
"  ARGS: 
     id  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_tick_callback" id)))

(define (GtkWidget-render-icon self stock-id size detail)
"  ARGS: 
     stock-id  - string, 
     size  - exact integer of size gint32, 
     detail  - #f for NULL or string
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon" stock-id size detail)))

(define (GtkWidget-render-icon-pixbuf self stock-id size)
"  ARGS: 
     stock-id  - string, 
     size  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "render_icon_pixbuf" stock-id size)))

(define (GtkWidget-reparent self new-parent)
"  ARGS: 
     new-parent  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reparent" new-parent)))

(define (GtkWidget-reset-rc-styles self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_rc_styles")))

(define (GtkWidget-reset-style self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reset_style")))

(define (GtkWidget-send-expose self event)
"  ARGS: 
     event  - union Event
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "send_expose" event)))

(define (GtkWidget-send-focus-change? self event)
"  ARGS: 
     event  - union Event
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "send_focus_change" event)))

(define (GtkWidget-set-accel-path self accel-path accel-group)
"  ARGS: 
     accel-path  - #f for NULL or string, 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accel_path" accel-path accel-group)))

(define (GtkWidget-set-allocation self allocation)
"  ARGS: 
     allocation  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_allocation" allocation)))

(define (GtkWidget-set-app-paintable self app-paintable)
"  ARGS: 
     app-paintable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_app_paintable" app-paintable)))

(define (GtkWidget-set-can-default self can-default)
"  ARGS: 
     can-default  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_can_default" can-default)))

(define (GtkWidget-set-can-focus self can-focus)
"  ARGS: 
     can-focus  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_can_focus" can-focus)))

(define (GtkWidget-set-child-visible self is-visible)
"  ARGS: 
     is-visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_child_visible" is-visible)))

(define (GtkWidget-set-clip self clip)
"  ARGS: 
     clip  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_clip" clip)))

(define (GtkWidget-set-composite-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_composite_name" name)))

(define (GtkWidget-set-device-enabled self device enabled)
"  ARGS: 
     device  - object Device, 
     enabled  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_device_enabled" device enabled)))

(define (GtkWidget-set-device-events self device events)
"  ARGS: 
     device  - object Device, 
     events  - exact integer of flags type EventMask
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_device_events" device events)))

(define (GtkWidget-set-direction self dir)
"  ARGS: 
     dir  - exact integer of enum type TextDirection
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_direction" dir)))

(define (GtkWidget-set-double-buffered self double-buffered)
"  ARGS: 
     double-buffered  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_double_buffered" double-buffered)))

(define (GtkWidget-set-events self events)
"  ARGS: 
     events  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_events" events)))

(define (GtkWidget-set-focus-on-click self focus-on-click)
"  ARGS: 
     focus-on-click  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_on_click" focus-on-click)))

(define (GtkWidget-set-font-map self font-map)
"  ARGS: 
     font-map  - object FontMap
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_font_map" font-map)))

(define (GtkWidget-set-font-options self options)
"  ARGS: 
     options  - struct FontOptions
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_font_options" options)))

(define (GtkWidget-set-halign self align)
"  ARGS: 
     align  - exact integer of enum type Align
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_halign" align)))

(define (GtkWidget-set-has-tooltip self has-tooltip)
"  ARGS: 
     has-tooltip  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_tooltip" has-tooltip)))

(define (GtkWidget-set-has-window self has-window)
"  ARGS: 
     has-window  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_window" has-window)))

(define (GtkWidget-set-hexpand self expand)
"  ARGS: 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hexpand" expand)))

(define (GtkWidget-set-hexpand-set self set)
"  ARGS: 
     set  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hexpand_set" set)))

(define (GtkWidget-set-mapped self mapped)
"  ARGS: 
     mapped  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mapped" mapped)))

(define (GtkWidget-set-margin-bottom self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_bottom" margin)))

(define (GtkWidget-set-margin-end self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_end" margin)))

(define (GtkWidget-set-margin-left self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_left" margin)))

(define (GtkWidget-set-margin-right self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_right" margin)))

(define (GtkWidget-set-margin-start self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_start" margin)))

(define (GtkWidget-set-margin-top self margin)
"  ARGS: 
     margin  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_margin_top" margin)))

(define (GtkWidget-set-name self name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_name" name)))

(define (GtkWidget-set-no-show-all self no-show-all)
"  ARGS: 
     no-show-all  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_no_show_all" no-show-all)))

(define (GtkWidget-set-opacity self opacity)
"  ARGS: 
     opacity  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_opacity" opacity)))

(define (GtkWidget-set-parent self parent)
"  ARGS: 
     parent  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_parent" parent)))

(define (GtkWidget-set-parent-window self parent-window)
"  ARGS: 
     parent-window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_parent_window" parent-window)))

(define (GtkWidget-set-realized self realized)
"  ARGS: 
     realized  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_realized" realized)))

(define (GtkWidget-set-receives-default self receives-default)
"  ARGS: 
     receives-default  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_receives_default" receives-default)))

(define (GtkWidget-set-redraw-on-allocate self redraw-on-allocate)
"  ARGS: 
     redraw-on-allocate  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_redraw_on_allocate" redraw-on-allocate)))

(define (GtkWidget-set-sensitive self sensitive)
"  ARGS: 
     sensitive  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_sensitive" sensitive)))

(define (GtkWidget-set-size-request self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_size_request" width height)))

(define (GtkWidget-set-state self state)
"  ARGS: 
     state  - exact integer of enum type StateType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state" state)))

(define (GtkWidget-set-state-flags self flags clear)
"  ARGS: 
     flags  - exact integer of flags type StateFlags, 
     clear  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_state_flags" flags clear)))

(define (GtkWidget-set-style self style)
"  ARGS: 
     style  - object Style
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_style" style)))

(define (GtkWidget-set-support-multidevice self support-multidevice)
"  ARGS: 
     support-multidevice  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_support_multidevice" support-multidevice)))

(define (GtkWidget-set-tooltip-markup self markup)
"  ARGS: 
     markup  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_markup" markup)))

(define (GtkWidget-set-tooltip-text self text)
"  ARGS: 
     text  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_text" text)))

(define (GtkWidget-set-tooltip-window self custom-window)
"  ARGS: 
     custom-window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_tooltip_window" custom-window)))

(define (GtkWidget-set-valign self align)
"  ARGS: 
     align  - exact integer of enum type Align
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_valign" align)))

(define (GtkWidget-set-vexpand self expand)
"  ARGS: 
     expand  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vexpand" expand)))

(define (GtkWidget-set-vexpand-set self set)
"  ARGS: 
     set  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_vexpand_set" set)))

(define (GtkWidget-set-visible self visible)
"  ARGS: 
     visible  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visible" visible)))

(define (GtkWidget-set-visual self visual)
"  ARGS: 
     visual  - object Visual
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_visual" visual)))

(define (GtkWidget-set-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_window" window)))

(define (GtkWidget-shape-combine-region self region)
"  ARGS: 
     region  - struct Region
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "shape_combine_region" region)))

(define (GtkWidget-show self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "show")))

(define (GtkWidget-show-all self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "show_all")))

(define (GtkWidget-show-now self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "show_now")))

(define (GtkWidget-size-allocate self allocation)
"  ARGS: 
     allocation  - struct Rectangle
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "size_allocate" allocation)))

(define (GtkWidget-size-allocate-with-baseline self allocation baseline)
"  ARGS: 
     allocation  - struct Rectangle, 
     baseline  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "size_allocate_with_baseline" allocation baseline)))

(define (GtkWidget-size-request self out-requisition)
"  ARGS: 
   RETURN: void
     requisition  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "size_request" out-requisition)))

(define (GtkWidget-style-attach self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "style_attach")))

(define (GtkWidget-style-get-property self property-name value)
"  ARGS: 
     property-name  - string, 
     value  - struct Value
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "style_get_property" property-name value)))

(define (GtkWidget-thaw-child-notify self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "thaw_child_notify")))

(define (GtkWidget-translate-coordinates? self dest-widget src-x src-y)
"  ARGS: 
     dest-widget  - object Widget, 
     src-x  - exact integer of size gint32, 
     src-y  - exact integer of size gint32, 
     dest-x  - exact integer of size gint32[OUT], 
     dest-y  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "translate_coordinates" dest-widget src-x src-y)))

(define (GtkWidget-trigger-tooltip-query self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "trigger_tooltip_query")))

(define (GtkWidget-unmap self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unmap")))

(define (GtkWidget-unparent self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unparent")))

(define (GtkWidget-unrealize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unrealize")))

(define (GtkWidget-unregister-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unregister_window" window)))

(define (GtkWidget-unset-state-flags self flags)
"  ARGS: 
     flags  - exact integer of flags type StateFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unset_state_flags" flags)))

(define <GtkWidgetAccessible>
  (gi-lookup-type "Gtk-WidgetAccessible"))

(define WIDGET_HELP_TYPE_TOOLTIP
  (gi-enum-value "Gtk-WidgetHelpType" "tooltip"))

(define WIDGET_HELP_TYPE_WHATS_THIS
  (gi-enum-value "Gtk-WidgetHelpType" "whats_this"))

(define <GtkWidgetPath>
  (gi-lookup-type "Gtk-WidgetPath"))

(define (GtkWidgetPath-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-WidgetPath-new"))

(define (GtkWidgetPath-append-for-widget self widget)
"  ARGS: 
     widget  - object Widget
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_for_widget" widget)))

(define (GtkWidgetPath-append-type self type)
"  ARGS: 
     type  - <GType>
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_type" type)))

(define (GtkWidgetPath-append-with-siblings self siblings sibling-index)
"  ARGS: 
     siblings  - struct WidgetPath, 
     sibling-index  - exact integer of size guint32
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "append_with_siblings" siblings sibling-index)))

(define (GtkWidgetPath-copy self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "copy")))

(define (GtkWidgetPath-free self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "free")))

(define (GtkWidgetPath-get-object-type self)
"  ARGS: 
   RETURN: GType
"
  (gi-method-send self 
     (gi-method-prepare "get_object_type")))

(define (GtkWidgetPath-has-parent? self type)
"  ARGS: 
     type  - <GType>
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_parent" type)))

(define (GtkWidgetPath-is-type? self type)
"  ARGS: 
     type  - <GType>
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_type" type)))

(define (GtkWidgetPath-iter-add-class self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_add_class" pos name)))

(define (GtkWidgetPath-iter-add-region self pos name flags)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string, 
     flags  - exact integer of flags type RegionFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_add_region" pos name flags)))

(define (GtkWidgetPath-iter-clear-classes self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_clear_classes" pos)))

(define (GtkWidgetPath-iter-clear-regions self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_clear_regions" pos)))

(define (GtkWidgetPath-iter-get-name self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_name" pos)))

(define (GtkWidgetPath-iter-get-object-name self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_object_name" pos)))

(define (GtkWidgetPath-iter-get-object-type self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: GType
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_object_type" pos)))

(define (GtkWidgetPath-iter-get-sibling-index self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: guint32
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_sibling_index" pos)))

(define (GtkWidgetPath-iter-get-siblings self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_siblings" pos)))

(define (GtkWidgetPath-iter-get-state self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "iter_get_state" pos)))

(define (GtkWidgetPath-iter-has-class? self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_class" pos name)))

(define (GtkWidgetPath-iter-has-name? self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_name" pos name)))

(define (GtkWidgetPath-iter-has-qclass? self pos qname)
"  ARGS: 
     pos  - exact integer of size gint32, 
     qname  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_qclass" pos qname)))

(define (GtkWidgetPath-iter-has-qname? self pos qname)
"  ARGS: 
     pos  - exact integer of size gint32, 
     qname  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_qname" pos qname)))

(define (GtkWidgetPath-iter-has-qregion? self pos qname)
"  ARGS: 
     pos  - exact integer of size gint32, 
     qname  - exact integer of size guint32, 
     flags  - exact integer of flags type RegionFlags[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_qregion" pos qname)))

(define (GtkWidgetPath-iter-has-region? self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string, 
     flags  - exact integer of flags type RegionFlags[OUT]
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "iter_has_region" pos name)))

(define (GtkWidgetPath-iter-list-classes self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "iter_list_classes" pos)))

(define (GtkWidgetPath-iter-list-regions self pos)
"  ARGS: 
     pos  - exact integer of size gint32
   RETURN: gslist*
"
  (gi-method-send self 
     (gi-method-prepare "iter_list_regions" pos)))

(define (GtkWidgetPath-iter-remove-class self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_remove_class" pos name)))

(define (GtkWidgetPath-iter-remove-region self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_remove_region" pos name)))

(define (GtkWidgetPath-iter-set-name self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_set_name" pos name)))

(define (GtkWidgetPath-iter-set-object-name self pos name)
"  ARGS: 
     pos  - exact integer of size gint32, 
     name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_set_object_name" pos name)))

(define (GtkWidgetPath-iter-set-object-type self pos type)
"  ARGS: 
     pos  - exact integer of size gint32, 
     type  - <GType>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_set_object_type" pos type)))

(define (GtkWidgetPath-iter-set-state self pos state)
"  ARGS: 
     pos  - exact integer of size gint32, 
     state  - exact integer of flags type StateFlags
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iter_set_state" pos state)))

(define (GtkWidgetPath-length self)
"  ARGS: 
   RETURN: gint32
"
  (gi-method-send self 
     (gi-method-prepare "length")))

(define (GtkWidgetPath-prepend-type self type)
"  ARGS: 
     type  - <GType>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "prepend_type" type)))

(define (GtkWidgetPath-ref self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "ref")))

(define (GtkWidgetPath-to-string self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "to_string")))

(define (GtkWidgetPath-unref self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unref")))

(define <GtkWindow>
  (gi-lookup-type "Gtk-Window"))

(define (GtkWindow-new type)
"  ARGS: 
     type  - exact integer of enum type WindowType
   RETURN: interface*
"
  (gi-function-invoke "Gtk-Window-new" type))

(define (GtkWindow-get-default-icon-list)
"  ARGS: 
   RETURN: glist*
"
  (gi-function-invoke "Gtk-Window-get_default_icon_list"))

(define (GtkWindow-get-default-icon-name)
"  ARGS: 
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-Window-get_default_icon_name"))

(define (GtkWindow-list-toplevels)
"  ARGS: 
   RETURN: glist*
"
  (gi-function-invoke "Gtk-Window-list_toplevels"))

(define (GtkWindow-set-auto-startup-notification setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-function-invoke "Gtk-Window-set_auto_startup_notification" setting))

(define (GtkWindow-set-default-icon icon)
"  ARGS: 
     icon  - object Pixbuf
   RETURN: void
"
  (gi-function-invoke "Gtk-Window-set_default_icon" icon))

(define (GtkWindow-set-default-icon-from-file? filename)
"  ARGS: 
     filename  - locale string
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-Window-set_default_icon_from_file" filename))

(define (GtkWindow-set-default-icon-list list)
"  ARGS: 
     list  - <GList>
   RETURN: void
"
  (gi-function-invoke "Gtk-Window-set_default_icon_list" list))

(define (GtkWindow-set-default-icon-name name)
"  ARGS: 
     name  - string
   RETURN: void
"
  (gi-function-invoke "Gtk-Window-set_default_icon_name" name))

(define (GtkWindow-set-interactive-debugging enable)
"  ARGS: 
     enable  - boolean
   RETURN: void
"
  (gi-function-invoke "Gtk-Window-set_interactive_debugging" enable))

(define (GtkWindow-activate-default? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate_default")))

(define (GtkWindow-activate-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate_focus")))

(define (GtkWindow-activate-key? self event)
"  ARGS: 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "activate_key" event)))

(define (GtkWindow-add-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_accel_group" accel-group)))

(define (GtkWindow-add-mnemonic self keyval target)
"  ARGS: 
     keyval  - exact integer of size guint32, 
     target  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_mnemonic" keyval target)))

(define (GtkWindow-begin-move-drag self button root-x root-y timestamp)
"  ARGS: 
     button  - exact integer of size gint32, 
     root-x  - exact integer of size gint32, 
     root-y  - exact integer of size gint32, 
     timestamp  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "begin_move_drag" button root-x root-y timestamp)))

(define (GtkWindow-begin-resize-drag self edge button root-x root-y timestamp)
"  ARGS: 
     edge  - exact integer of enum type WindowEdge, 
     button  - exact integer of size gint32, 
     root-x  - exact integer of size gint32, 
     root-y  - exact integer of size gint32, 
     timestamp  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "begin_resize_drag" edge button root-x root-y timestamp)))

(define (GtkWindow-close self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "close")))

(define (GtkWindow-deiconify self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "deiconify")))

(define (GtkWindow-fullscreen self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "fullscreen")))

(define (GtkWindow-fullscreen-on-monitor self screen monitor)
"  ARGS: 
     screen  - object Screen, 
     monitor  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "fullscreen_on_monitor" screen monitor)))

(define (GtkWindow-get-accept-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_accept_focus")))

(define (GtkWindow-get-application self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_application")))

(define (GtkWindow-get-attached-to self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_attached_to")))

(define (GtkWindow-get-decorated? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_decorated")))

(define (GtkWindow-get-default-size self)
"  ARGS: 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_default_size")))

(define (GtkWindow-get-default-widget self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_default_widget")))

(define (GtkWindow-get-deletable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_deletable")))

(define (GtkWindow-get-destroy-with-parent? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_destroy_with_parent")))

(define (GtkWindow-get-focus self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_focus")))

(define (GtkWindow-get-focus-on-map? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_on_map")))

(define (GtkWindow-get-focus-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_focus_visible")))

(define (GtkWindow-get-gravity self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_gravity")))

(define (GtkWindow-get-group self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_group")))

(define (GtkWindow-get-has-resize-grip? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_has_resize_grip")))

(define (GtkWindow-get-hide-titlebar-when-maximized? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_hide_titlebar_when_maximized")))

(define (GtkWindow-get-icon self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon")))

(define (GtkWindow-get-icon-list self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_list")))

(define (GtkWindow-get-icon-name self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_icon_name")))

(define (GtkWindow-get-mnemonic-modifier self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_mnemonic_modifier")))

(define (GtkWindow-get-mnemonics-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_mnemonics_visible")))

(define (GtkWindow-get-modal? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_modal")))

(define (GtkWindow-get-opacity self)
"  ARGS: 
   RETURN: gdouble
"
  (gi-method-send self 
     (gi-method-prepare "get_opacity")))

(define (GtkWindow-get-position self)
"  ARGS: 
     root-x  - exact integer of size gint32[OUT], 
     root-y  - exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_position")))

(define (GtkWindow-get-resizable? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_resizable")))

(define (GtkWindow-get-resize-grip-area? self out-rect)
"  ARGS: 
   RETURN: gboolean
     rect  - Unhandled argument type tag 16
"
  (gi-method-send self 
     (gi-method-prepare "get_resize_grip_area" out-rect)))

(define (GtkWindow-get-role self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_role")))

(define (GtkWindow-get-screen self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_screen")))

(define (GtkWindow-get-size self)
"  ARGS: 
     width  - #f for NULL or exact integer of size gint32[OUT], 
     height  - #f for NULL or exact integer of size gint32[OUT]
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "get_size")))

(define (GtkWindow-get-skip-pager-hint? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_skip_pager_hint")))

(define (GtkWindow-get-skip-taskbar-hint? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_skip_taskbar_hint")))

(define (GtkWindow-get-title self)
"  ARGS: 
   RETURN: utf8*
"
  (gi-method-send self 
     (gi-method-prepare "get_title")))

(define (GtkWindow-get-titlebar self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_titlebar")))

(define (GtkWindow-get-transient-for self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_transient_for")))

(define (GtkWindow-get-type-hint self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_type_hint")))

(define (GtkWindow-get-urgency-hint? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "get_urgency_hint")))

(define (GtkWindow-get-window-type self)
"  ARGS: 
   RETURN: interface
"
  (gi-method-send self 
     (gi-method-prepare "get_window_type")))

(define (GtkWindow-has-group? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_group")))

(define (GtkWindow-has-toplevel-focus? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "has_toplevel_focus")))

(define (GtkWindow-iconify self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "iconify")))

(define (GtkWindow-is-active? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_active")))

(define (GtkWindow-is-maximized? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "is_maximized")))

(define (GtkWindow-maximize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "maximize")))

(define (GtkWindow-mnemonic-activate? self keyval modifier)
"  ARGS: 
     keyval  - exact integer of size guint32, 
     modifier  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "mnemonic_activate" keyval modifier)))

(define (GtkWindow-move self x y)
"  ARGS: 
     x  - exact integer of size gint32, 
     y  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "move" x y)))

(define (GtkWindow-parse-geometry? self geometry)
"  ARGS: 
     geometry  - string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "parse_geometry" geometry)))

(define (GtkWindow-present self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "present")))

(define (GtkWindow-present-with-time self timestamp)
"  ARGS: 
     timestamp  - exact integer of size guint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "present_with_time" timestamp)))

(define (GtkWindow-propagate-key-event? self event)
"  ARGS: 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "propagate_key_event" event)))

(define (GtkWindow-remove-accel-group self accel-group)
"  ARGS: 
     accel-group  - object AccelGroup
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_accel_group" accel-group)))

(define (GtkWindow-remove-mnemonic self keyval target)
"  ARGS: 
     keyval  - exact integer of size guint32, 
     target  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_mnemonic" keyval target)))

(define (GtkWindow-reshow-with-initial-size self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "reshow_with_initial_size")))

(define (GtkWindow-resize self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "resize" width height)))

(define (GtkWindow-resize-grip-is-visible? self)
"  ARGS: 
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "resize_grip_is_visible")))

(define (GtkWindow-resize-to-geometry self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "resize_to_geometry" width height)))

(define (GtkWindow-set-accept-focus self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_accept_focus" setting)))

(define (GtkWindow-set-application self application)
"  ARGS: 
     application  - object Application
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_application" application)))

(define (GtkWindow-set-attached-to self attach-widget)
"  ARGS: 
     attach-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_attached_to" attach-widget)))

(define (GtkWindow-set-decorated self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_decorated" setting)))

(define (GtkWindow-set-default self default-widget)
"  ARGS: 
     default-widget  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default" default-widget)))

(define (GtkWindow-set-default-geometry self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_geometry" width height)))

(define (GtkWindow-set-default-size self width height)
"  ARGS: 
     width  - exact integer of size gint32, 
     height  - exact integer of size gint32
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_default_size" width height)))

(define (GtkWindow-set-deletable self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_deletable" setting)))

(define (GtkWindow-set-destroy-with-parent self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_destroy_with_parent" setting)))

(define (GtkWindow-set-focus self focus)
"  ARGS: 
     focus  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus" focus)))

(define (GtkWindow-set-focus-on-map self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_on_map" setting)))

(define (GtkWindow-set-focus-visible self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_focus_visible" setting)))

(define (GtkWindow-set-geometry-hints self geometry-widget geometry geom-mask)
"  ARGS: 
     geometry-widget  - object Widget, 
     geometry  - struct Geometry, 
     geom-mask  - exact integer of flags type WindowHints
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_geometry_hints" geometry-widget geometry geom-mask)))

(define (GtkWindow-set-gravity self gravity)
"  ARGS: 
     gravity  - exact integer of enum type Gravity
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_gravity" gravity)))

(define (GtkWindow-set-has-resize-grip self value)
"  ARGS: 
     value  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_resize_grip" value)))

(define (GtkWindow-set-has-user-ref-count self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_has_user_ref_count" setting)))

(define (GtkWindow-set-hide-titlebar-when-maximized self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_hide_titlebar_when_maximized" setting)))

(define (GtkWindow-set-icon self icon)
"  ARGS: 
     icon  - object Pixbuf
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon" icon)))

(define (GtkWindow-set-icon-from-file? self filename)
"  ARGS: 
     filename  - locale string
   RETURN: gboolean
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_from_file" filename)))

(define (GtkWindow-set-icon-list self list)
"  ARGS: 
     list  - <GList>
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_list" list)))

(define (GtkWindow-set-icon-name self name)
"  ARGS: 
     name  - #f for NULL or string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_icon_name" name)))

(define (GtkWindow-set-keep-above self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_keep_above" setting)))

(define (GtkWindow-set-keep-below self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_keep_below" setting)))

(define (GtkWindow-set-mnemonic-modifier self modifier)
"  ARGS: 
     modifier  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mnemonic_modifier" modifier)))

(define (GtkWindow-set-mnemonics-visible self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_mnemonics_visible" setting)))

(define (GtkWindow-set-modal self modal)
"  ARGS: 
     modal  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_modal" modal)))

(define (GtkWindow-set-opacity self opacity)
"  ARGS: 
     opacity  - real number of size gdouble
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_opacity" opacity)))

(define (GtkWindow-set-position self position)
"  ARGS: 
     position  - exact integer of enum type WindowPosition
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_position" position)))

(define (GtkWindow-set-resizable self resizable)
"  ARGS: 
     resizable  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_resizable" resizable)))

(define (GtkWindow-set-role self role)
"  ARGS: 
     role  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_role" role)))

(define (GtkWindow-set-screen self screen)
"  ARGS: 
     screen  - object Screen
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_screen" screen)))

(define (GtkWindow-set-skip-pager-hint self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_skip_pager_hint" setting)))

(define (GtkWindow-set-skip-taskbar-hint self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_skip_taskbar_hint" setting)))

(define (GtkWindow-set-startup-id self startup-id)
"  ARGS: 
     startup-id  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_startup_id" startup-id)))

(define (GtkWindow-set-title self title)
"  ARGS: 
     title  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_title" title)))

(define (GtkWindow-set-titlebar self titlebar)
"  ARGS: 
     titlebar  - object Widget
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_titlebar" titlebar)))

(define (GtkWindow-set-transient-for self parent)
"  ARGS: 
     parent  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_transient_for" parent)))

(define (GtkWindow-set-type-hint self hint)
"  ARGS: 
     hint  - exact integer of enum type WindowTypeHint
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_type_hint" hint)))

(define (GtkWindow-set-urgency-hint self setting)
"  ARGS: 
     setting  - boolean
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_urgency_hint" setting)))

(define (GtkWindow-set-wmclass self wmclass-name wmclass-class)
"  ARGS: 
     wmclass-name  - string, 
     wmclass-class  - string
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "set_wmclass" wmclass-name wmclass-class)))

(define (GtkWindow-stick self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "stick")))

(define (GtkWindow-unfullscreen self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unfullscreen")))

(define (GtkWindow-unmaximize self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unmaximize")))

(define (GtkWindow-unstick self)
"  ARGS: 
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "unstick")))

(define <GtkWindowAccessible>
  (gi-lookup-type "Gtk-WindowAccessible"))

(define <GtkWindowGroup>
  (gi-lookup-type "Gtk-WindowGroup"))

(define (GtkWindowGroup-new)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-WindowGroup-new"))

(define (GtkWindowGroup-add-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "add_window" window)))

(define (GtkWindowGroup-get-current-device-grab self device)
"  ARGS: 
     device  - object Device
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_current_device_grab" device)))

(define (GtkWindowGroup-get-current-grab self)
"  ARGS: 
   RETURN: interface*
"
  (gi-method-send self 
     (gi-method-prepare "get_current_grab")))

(define (GtkWindowGroup-list-windows self)
"  ARGS: 
   RETURN: glist*
"
  (gi-method-send self 
     (gi-method-prepare "list_windows")))

(define (GtkWindowGroup-remove-window self window)
"  ARGS: 
     window  - object Window
   RETURN: void
"
  (gi-method-send self 
     (gi-method-prepare "remove_window" window)))

(define WINDOW_POSITION_NONE
  (gi-enum-value "Gtk-WindowPosition" "none"))

(define WINDOW_POSITION_CENTER
  (gi-enum-value "Gtk-WindowPosition" "center"))

(define WINDOW_POSITION_MOUSE
  (gi-enum-value "Gtk-WindowPosition" "mouse"))

(define WINDOW_POSITION_CENTER_ALWAYS
  (gi-enum-value "Gtk-WindowPosition" "center_always"))

(define WINDOW_POSITION_CENTER_ON_PARENT
  (gi-enum-value "Gtk-WindowPosition" "center_on_parent"))

(define WINDOW_TYPE_TOPLEVEL
  (gi-enum-value "Gtk-WindowType" "toplevel"))

(define WINDOW_TYPE_POPUP
  (gi-enum-value "Gtk-WindowType" "popup"))

(define WRAP_MODE_NONE
  (gi-enum-value "Gtk-WrapMode" "none"))

(define WRAP_MODE_CHAR
  (gi-enum-value "Gtk-WrapMode" "char"))

(define WRAP_MODE_WORD
  (gi-enum-value "Gtk-WrapMode" "word"))

(define WRAP_MODE_WORD_CHAR
  (gi-enum-value "Gtk-WrapMode" "word_char"))

(define (gtk-accel-groups-activate object accel-key accel-mods)
"  ARGS: 
     object  - object Object, 
     accel-key  - exact integer of size guint32, 
     accel-mods  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-accel_groups_activate" object accel-key accel-mods))

(define (gtk-accel-groups-from-object object)
"  ARGS: 
     object  - object Object
   RETURN: gslist*
"
  (gi-function-invoke "Gtk-accel_groups_from_object" object))

(define (gtk-accelerator-get-default-mod-mask)
"  ARGS: 
   RETURN: interface
"
  (gi-function-invoke "Gtk-accelerator_get_default_mod_mask"))

(define (gtk-accelerator-get-label accelerator-key accelerator-mods)
"  ARGS: 
     accelerator-key  - exact integer of size guint32, 
     accelerator-mods  - exact integer of flags type ModifierType
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-accelerator_get_label" accelerator-key accelerator-mods))

(define (gtk-accelerator-get-label-with-keycode display accelerator-key keycode accelerator-mods)
"  ARGS: 
     display  - object Display, 
     accelerator-key  - exact integer of size guint32, 
     keycode  - exact integer of size guint32, 
     accelerator-mods  - exact integer of flags type ModifierType
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-accelerator_get_label_with_keycode" display accelerator-key keycode accelerator-mods))

(define (gtk-accelerator-name accelerator-key accelerator-mods)
"  ARGS: 
     accelerator-key  - exact integer of size guint32, 
     accelerator-mods  - exact integer of flags type ModifierType
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-accelerator_name" accelerator-key accelerator-mods))

(define (gtk-accelerator-name-with-keycode display accelerator-key keycode accelerator-mods)
"  ARGS: 
     display  - object Display, 
     accelerator-key  - exact integer of size guint32, 
     keycode  - exact integer of size guint32, 
     accelerator-mods  - exact integer of flags type ModifierType
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-accelerator_name_with_keycode" display accelerator-key keycode accelerator-mods))

(define (gtk-accelerator-parse accelerator)
"  ARGS: 
     accelerator  - string, 
     accelerator-key  - exact integer of size guint32[OUT], 
     accelerator-mods  - exact integer of flags type ModifierType[OUT]
   RETURN: void
"
  (gi-function-invoke "Gtk-accelerator_parse" accelerator))

(define (gtk-accelerator-parse-with-keycode accelerator)
"  ARGS: 
     accelerator  - string, 
     accelerator-key  - exact integer of size guint32[OUT], 
     accelerator-codes  - Unhandled argument type tag 15[OUT], 
     accelerator-mods  - exact integer of flags type ModifierType[OUT]
   RETURN: void
"
  (gi-function-invoke "Gtk-accelerator_parse_with_keycode" accelerator))

(define (gtk-accelerator-set-default-mod-mask default-mod-mask)
"  ARGS: 
     default-mod-mask  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-function-invoke "Gtk-accelerator_set_default_mod_mask" default-mod-mask))

(define (gtk-accelerator-valid keyval modifiers)
"  ARGS: 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-accelerator_valid" keyval modifiers))

(define (gtk-binding-entry-add-signal-from-string binding-set signal-desc)
"  ARGS: 
     binding-set  - struct BindingSet, 
     signal-desc  - string
   RETURN: interface
"
  (gi-function-invoke "Gtk-binding_entry_add_signal_from_string" binding-set signal-desc))

(define (gtk-binding-entry-add-signall binding-set keyval modifiers signal-name binding-args)
"  ARGS: 
     binding-set  - struct BindingSet, 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType, 
     signal-name  - string, 
     binding-args  - <GSList>
   RETURN: void
"
  (gi-function-invoke "Gtk-binding_entry_add_signall" binding-set keyval modifiers signal-name binding-args))

(define (gtk-binding-entry-remove binding-set keyval modifiers)
"  ARGS: 
     binding-set  - struct BindingSet, 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-function-invoke "Gtk-binding_entry_remove" binding-set keyval modifiers))

(define (gtk-binding-entry-skip binding-set keyval modifiers)
"  ARGS: 
     binding-set  - struct BindingSet, 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType
   RETURN: void
"
  (gi-function-invoke "Gtk-binding_entry_skip" binding-set keyval modifiers))

(define (gtk-binding-set-find set-name)
"  ARGS: 
     set-name  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-binding_set_find" set-name))

(define (gtk-bindings-activate object keyval modifiers)
"  ARGS: 
     object  - object Object, 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-bindings_activate" object keyval modifiers))

(define (gtk-bindings-activate-event object event)
"  ARGS: 
     object  - object Object, 
     event  - struct EventKey
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-bindings_activate_event" object event))

(define (gtk-builder-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-builder_error_quark"))

(define (gtk-cairo-should-draw-window cr window)
"  ARGS: 
     cr  - struct Context, 
     window  - object Window
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-cairo_should_draw_window" cr window))

(define (gtk-cairo-transform-to-window cr widget window)
"  ARGS: 
     cr  - struct Context, 
     widget  - object Widget, 
     window  - object Window
   RETURN: void
"
  (gi-function-invoke "Gtk-cairo_transform_to_window" cr widget window))

(define (gtk-check-version required-major required-minor required-micro)
"  ARGS: 
     required-major  - exact integer of size guint32, 
     required-minor  - exact integer of size guint32, 
     required-micro  - exact integer of size guint32
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-check_version" required-major required-minor required-micro))

(define (gtk-css-provider-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-css_provider_error_quark"))

(define (gtk-device-grab-add widget device block-others)
"  ARGS: 
     widget  - object Widget, 
     device  - object Device, 
     block-others  - boolean
   RETURN: void
"
  (gi-function-invoke "Gtk-device_grab_add" widget device block-others))

(define (gtk-device-grab-remove widget device)
"  ARGS: 
     widget  - object Widget, 
     device  - object Device
   RETURN: void
"
  (gi-function-invoke "Gtk-device_grab_remove" widget device))

(define (gtk-disable-setlocale)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-disable_setlocale"))

(define (gtk-distribute-natural-allocation extra-space n-requested-sizes sizes)
"  ARGS: 
     extra-space  - exact integer of size gint32, 
     n-requested-sizes  - exact integer of size guint32, 
     sizes  - struct RequestedSize
   RETURN: gint32
"
  (gi-function-invoke "Gtk-distribute_natural_allocation" extra-space n-requested-sizes sizes))

(define (gtk-drag-cancel context)
"  ARGS: 
     context  - object DragContext
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_cancel" context))

(define (gtk-drag-finish context success del time-)
"  ARGS: 
     context  - object DragContext, 
     success  - boolean, 
     del  - boolean, 
     time-  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_finish" context success del time-))

(define (gtk-drag-get-source-widget context)
"  ARGS: 
     context  - object DragContext
   RETURN: interface*
"
  (gi-function-invoke "Gtk-drag_get_source_widget" context))

(define (gtk-drag-set-icon-default context)
"  ARGS: 
     context  - object DragContext
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_default" context))

(define (gtk-drag-set-icon-gicon context icon hot-x hot-y)
"  ARGS: 
     context  - object DragContext, 
     icon  - Unhandled argument type tag 16, 
     hot-x  - exact integer of size gint32, 
     hot-y  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_gicon" context icon hot-x hot-y))

(define (gtk-drag-set-icon-name context icon-name hot-x hot-y)
"  ARGS: 
     context  - object DragContext, 
     icon-name  - string, 
     hot-x  - exact integer of size gint32, 
     hot-y  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_name" context icon-name hot-x hot-y))

(define (gtk-drag-set-icon-pixbuf context pixbuf hot-x hot-y)
"  ARGS: 
     context  - object DragContext, 
     pixbuf  - object Pixbuf, 
     hot-x  - exact integer of size gint32, 
     hot-y  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_pixbuf" context pixbuf hot-x hot-y))

(define (gtk-drag-set-icon-surface context surface)
"  ARGS: 
     context  - object DragContext, 
     surface  - struct Surface
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_surface" context surface))

(define (gtk-drag-set-icon-widget context widget hot-x hot-y)
"  ARGS: 
     context  - object DragContext, 
     widget  - object Widget, 
     hot-x  - exact integer of size gint32, 
     hot-y  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-drag_set_icon_widget" context widget hot-x hot-y))

(define (gtk-events-pending)
"  ARGS: 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-events_pending"))

(define (gtk-false)
"  ARGS: 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-false"))

(define (gtk-file-chooser-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-file_chooser_error_quark"))

(define (gtk-get-binary-age)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_binary_age"))

(define (gtk-get-current-event)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-get_current_event"))

(define (gtk-get-current-event-device)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-get_current_event_device"))

(define (gtk-get-current-event-state)
"  ARGS: 
     state  - exact integer of flags type ModifierType[OUT]
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-get_current_event_state"))

(define (gtk-get-current-event-time)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_current_event_time"))

(define (gtk-get-debug-flags)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_debug_flags"))

(define (gtk-get-default-language)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-get_default_language"))

(define (gtk-get-event-widget event)
"  ARGS: 
     event  - union Event
   RETURN: interface*
"
  (gi-function-invoke "Gtk-get_event_widget" event))

(define (gtk-get-interface-age)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_interface_age"))

(define (gtk-get-locale-direction)
"  ARGS: 
   RETURN: interface
"
  (gi-function-invoke "Gtk-get_locale_direction"))

(define (gtk-get-major-version)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_major_version"))

(define (gtk-get-micro-version)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_micro_version"))

(define (gtk-get-minor-version)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-get_minor_version"))

(define (gtk-get-option-group open-default-display)
"  ARGS: 
     open-default-display  - boolean
   RETURN: interface*
"
  (gi-function-invoke "Gtk-get_option_group" open-default-display))

(define (gtk-grab-get-current)
"  ARGS: 
   RETURN: interface*
"
  (gi-function-invoke "Gtk-grab_get_current"))

(define (gtk-icon-size-lookup size)
"  ARGS: 
     size  - exact integer of size gint32, 
     width  - exact integer of size gint32[OUT], 
     height  - exact integer of size gint32[OUT]
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-icon_size_lookup" size))

(define (gtk-icon-theme-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-icon_theme_error_quark"))

(define (gtk-init argc argv)
"  ARGS: 
     argc  - exact integer of size gint32[INOUT] , 
     argv  - #f for NULL or Unhandled argument type tag 15[INOUT] 
   RETURN: void
"
  (gi-function-invoke "Gtk-init" argc argv))

(define (gtk-init-abi-check argc argv num-checks sizeof-gtk-window sizeof-gtk-box)
"  ARGS: 
     argc  - bytevector containing elements gint32, 
     argv  - string, 
     num-checks  - exact integer of size gint32, 
     sizeof-gtk-window  - exact integer of size guint32, 
     sizeof-gtk-box  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-init_abi_check" argc argv num-checks sizeof-gtk-window sizeof-gtk-box))

(define (gtk-init-check argc argv)
"  ARGS: 
     argc  - exact integer of size gint32[INOUT] , 
     argv  - #f for NULL or Unhandled argument type tag 15[INOUT] 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-init_check" argc argv))

(define (gtk-init-check-abi-check argc argv num-checks sizeof-gtk-window sizeof-gtk-box)
"  ARGS: 
     argc  - bytevector containing elements gint32, 
     argv  - string, 
     num-checks  - exact integer of size gint32, 
     sizeof-gtk-window  - exact integer of size guint32, 
     sizeof-gtk-box  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-init_check_abi_check" argc argv num-checks sizeof-gtk-window sizeof-gtk-box))

(define (gtk-init-with-args argc argv parameter-string entries translation-domain)
"  ARGS: 
     argc  - exact integer of size gint32[INOUT] , 
     argv  - #f for NULL or Unhandled argument type tag 15[INOUT] , 
     parameter-string  - #f for NULL or string, 
     entries  - Unhandled argument type tag 15, 
     translation-domain  - #f for NULL or string
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-init_with_args" argc argv parameter-string entries translation-domain))

(define (gtk-main)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-main"))

(define (gtk-main-do-event event)
"  ARGS: 
     event  - union Event
   RETURN: void
"
  (gi-function-invoke "Gtk-main_do_event" event))

(define (gtk-main-iteration)
"  ARGS: 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-main_iteration"))

(define (gtk-main-iteration-do blocking)
"  ARGS: 
     blocking  - boolean
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-main_iteration_do" blocking))

(define (gtk-main-level)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-main_level"))

(define (gtk-main-quit)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-main_quit"))

(define (gtk-paper-size-get-default)
"  ARGS: 
   RETURN: utf8*
"
  (gi-function-invoke "Gtk-paper_size_get_default"))

(define (gtk-paper-size-get-paper-sizes include-custom)
"  ARGS: 
     include-custom  - boolean
   RETURN: glist*
"
  (gi-function-invoke "Gtk-paper_size_get_paper_sizes" include-custom))

(define (gtk-parse-args argc argv)
"  ARGS: 
     argc  - exact integer of size gint32[INOUT] , 
     argv  - Unhandled argument type tag 15[INOUT] 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-parse_args" argc argv))

(define (gtk-print-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-print_error_quark"))

(define (gtk-print-run-page-setup-dialog parent page-setup settings)
"  ARGS: 
     parent  - object Window, 
     page-setup  - object PageSetup, 
     settings  - object PrintSettings
   RETURN: interface*
"
  (gi-function-invoke "Gtk-print_run_page_setup_dialog" parent page-setup settings))

(define (gtk-print-run-page-setup-dialog-async parent page-setup settings done-cb data)
"  ARGS: 
     parent  - object Window, 
     page-setup  - object PageSetup, 
     settings  - object PrintSettings, 
     done-cb  - procedure of type PageSetupDoneFunc, 
     data  - #f for NULL or pointer
   RETURN: void
"
  (gi-function-invoke "Gtk-print_run_page_setup_dialog_async" parent page-setup settings done-cb data))

(define (gtk-propagate-event widget event)
"  ARGS: 
     widget  - object Widget, 
     event  - union Event
   RETURN: void
"
  (gi-function-invoke "Gtk-propagate_event" widget event))

(define (gtk-rc-property-parse-border pspec gstring property-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     gstring  - struct String, 
     property-value  - struct Value
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-rc_property_parse_border" pspec gstring property-value))

(define (gtk-rc-property-parse-color pspec gstring property-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     gstring  - struct String, 
     property-value  - struct Value
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-rc_property_parse_color" pspec gstring property-value))

(define (gtk-rc-property-parse-enum pspec gstring property-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     gstring  - struct String, 
     property-value  - struct Value
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-rc_property_parse_enum" pspec gstring property-value))

(define (gtk-rc-property-parse-flags pspec gstring property-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     gstring  - struct String, 
     property-value  - struct Value
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-rc_property_parse_flags" pspec gstring property-value))

(define (gtk-rc-property-parse-requisition pspec gstring property-value)
"  ARGS: 
     pspec  - object ParamSpec, 
     gstring  - struct String, 
     property-value  - struct Value
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-rc_property_parse_requisition" pspec gstring property-value))

(define (gtk-recent-chooser-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-recent_chooser_error_quark"))

(define (gtk-recent-manager-error-quark)
"  ARGS: 
   RETURN: guint32
"
  (gi-function-invoke "Gtk-recent_manager_error_quark"))

(define (gtk-render-activity context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_activity" context cr x y width height))

(define (gtk-render-arrow context cr angle x y size)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     angle  - real number of size gdouble, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     size  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_arrow" context cr angle x y size))

(define (gtk-render-background context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_background" context cr x y width height))

(define (gtk-render-background-get-clip context x y width height out-out-clip)
"  ARGS: 
     context  - object StyleContext, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble, 
   RETURN: void
     out-clip  - Unhandled argument type tag 16
"
  (gi-function-invoke "Gtk-render_background_get_clip" context x y width height out-out-clip))

(define (gtk-render-check context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_check" context cr x y width height))

(define (gtk-render-expander context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_expander" context cr x y width height))

(define (gtk-render-extension context cr x y width height gap-side)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble, 
     gap-side  - exact integer of enum type PositionType
   RETURN: void
"
  (gi-function-invoke "Gtk-render_extension" context cr x y width height gap-side))

(define (gtk-render-focus context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_focus" context cr x y width height))

(define (gtk-render-frame context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_frame" context cr x y width height))

(define (gtk-render-handle context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_handle" context cr x y width height))

(define (gtk-render-icon context cr pixbuf x y)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     pixbuf  - object Pixbuf, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_icon" context cr pixbuf x y))

(define (gtk-render-icon-surface context cr surface x y)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     surface  - struct Surface, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_icon_surface" context cr surface x y))

(define (gtk-render-insertion-cursor context cr x y layout index direction)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     layout  - object Layout, 
     index  - exact integer of size gint32, 
     direction  - exact integer of enum type Direction
   RETURN: void
"
  (gi-function-invoke "Gtk-render_insertion_cursor" context cr x y layout index direction))

(define (gtk-render-layout context cr x y layout)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     layout  - object Layout
   RETURN: void
"
  (gi-function-invoke "Gtk-render_layout" context cr x y layout))

(define (gtk-render-line context cr x0 y0 x1 y1)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x0  - real number of size gdouble, 
     y0  - real number of size gdouble, 
     x1  - real number of size gdouble, 
     y1  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_line" context cr x0 y0 x1 y1))

(define (gtk-render-option context cr x y width height)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble
   RETURN: void
"
  (gi-function-invoke "Gtk-render_option" context cr x y width height))

(define (gtk-render-slider context cr x y width height orientation)
"  ARGS: 
     context  - object StyleContext, 
     cr  - struct Context, 
     x  - real number of size gdouble, 
     y  - real number of size gdouble, 
     width  - real number of size gdouble, 
     height  - real number of size gdouble, 
     orientation  - exact integer of enum type Orientation
   RETURN: void
"
  (gi-function-invoke "Gtk-render_slider" context cr x y width height orientation))

(define (gtk-rgb-to-hsv r g b)
"  ARGS: 
     r  - real number of size gdouble, 
     g  - real number of size gdouble, 
     b  - real number of size gdouble, 
     h  - real number of size gdouble[OUT], 
     s  - real number of size gdouble[OUT], 
     v  - real number of size gdouble[OUT]
   RETURN: void
"
  (gi-function-invoke "Gtk-rgb_to_hsv" r g b))

(define (gtk-selection-add-target widget selection target info)
"  ARGS: 
     widget  - object Widget, 
     selection  - struct Atom, 
     target  - struct Atom, 
     info  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-selection_add_target" widget selection target info))

(define (gtk-selection-add-targets widget selection targets ntargets)
"  ARGS: 
     widget  - object Widget, 
     selection  - struct Atom, 
     targets  - Unhandled argument type tag 15, 
     ntargets  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-selection_add_targets" widget selection targets ntargets))

(define (gtk-selection-clear-targets widget selection)
"  ARGS: 
     widget  - object Widget, 
     selection  - struct Atom
   RETURN: void
"
  (gi-function-invoke "Gtk-selection_clear_targets" widget selection))

(define (gtk-selection-convert widget selection target time-)
"  ARGS: 
     widget  - object Widget, 
     selection  - struct Atom, 
     target  - struct Atom, 
     time-  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-selection_convert" widget selection target time-))

(define (gtk-selection-owner-set widget selection time-)
"  ARGS: 
     widget  - object Widget, 
     selection  - struct Atom, 
     time-  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-selection_owner_set" widget selection time-))

(define (gtk-selection-owner-set-for-display display widget selection time-)
"  ARGS: 
     display  - object Display, 
     widget  - object Widget, 
     selection  - struct Atom, 
     time-  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-selection_owner_set_for_display" display widget selection time-))

(define (gtk-selection-remove-all widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-function-invoke "Gtk-selection_remove_all" widget))

(define (gtk-set-debug-flags flags)
"  ARGS: 
     flags  - exact integer of size guint32
   RETURN: void
"
  (gi-function-invoke "Gtk-set_debug_flags" flags))

(define (gtk-show-uri screen uri timestamp)
"  ARGS: 
     screen  - object Screen, 
     uri  - string, 
     timestamp  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-show_uri" screen uri timestamp))

(define (gtk-show-uri-on-window parent uri timestamp)
"  ARGS: 
     parent  - object Window, 
     uri  - string, 
     timestamp  - exact integer of size guint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-show_uri_on_window" parent uri timestamp))

(define (gtk-target-table-free targets n-targets)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32
   RETURN: void
"
  (gi-function-invoke "Gtk-target_table_free" targets n-targets))

(define (gtk-target-table-new-from-list list)
"  ARGS: 
     list  - struct TargetList, 
     n-targets  - exact integer of size gint32[OUT]
   RETURN: array*
"
  (gi-function-invoke "Gtk-target_table_new_from_list" list))

(define (gtk-targets-include-image targets n-targets writable)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     writable  - boolean
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-targets_include_image" targets n-targets writable))

(define (gtk-targets-include-rich-text targets n-targets buffer)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32, 
     buffer  - object TextBuffer
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-targets_include_rich_text" targets n-targets buffer))

(define (gtk-targets-include-text targets n-targets)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-targets_include_text" targets n-targets))

(define (gtk-targets-include-uri targets n-targets)
"  ARGS: 
     targets  - Unhandled argument type tag 15, 
     n-targets  - exact integer of size gint32
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-targets_include_uri" targets n-targets))

(define (gtk-test-find-label widget label-pattern)
"  ARGS: 
     widget  - object Widget, 
     label-pattern  - string
   RETURN: interface*
"
  (gi-function-invoke "Gtk-test_find_label" widget label-pattern))

(define (gtk-test-find-sibling base-widget widget-type)
"  ARGS: 
     base-widget  - object Widget, 
     widget-type  - <GType>
   RETURN: interface*
"
  (gi-function-invoke "Gtk-test_find_sibling" base-widget widget-type))

(define (gtk-test-find-widget widget label-pattern widget-type)
"  ARGS: 
     widget  - object Widget, 
     label-pattern  - string, 
     widget-type  - <GType>
   RETURN: interface*
"
  (gi-function-invoke "Gtk-test_find_widget" widget label-pattern widget-type))

(define (gtk-test-list-all-types)
"  ARGS: 
     n-types  - exact integer of size guint32[OUT]
   RETURN: array*
"
  (gi-function-invoke "Gtk-test_list_all_types"))

(define (gtk-test-register-all-types)
"  ARGS: 
   RETURN: void
"
  (gi-function-invoke "Gtk-test_register_all_types"))

(define (gtk-test-widget-send-key widget keyval modifiers)
"  ARGS: 
     widget  - object Widget, 
     keyval  - exact integer of size guint32, 
     modifiers  - exact integer of flags type ModifierType
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-test_widget_send_key" widget keyval modifiers))

(define (gtk-test-widget-wait-for-draw widget)
"  ARGS: 
     widget  - object Widget
   RETURN: void
"
  (gi-function-invoke "Gtk-test_widget_wait_for_draw" widget))

(define (gtk-tree-get-row-drag-data selection-data)
"  ARGS: 
     selection-data  - struct SelectionData, 
     tree-model  - #f for NULL or Unhandled argument type tag 16[OUT], 
     path  - struct TreePath[OUT]
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-tree_get_row_drag_data" selection-data))

(define (gtk-tree-row-reference-deleted proxy path)
"  ARGS: 
     proxy  - object Object, 
     path  - struct TreePath
   RETURN: void
"
  (gi-function-invoke "Gtk-tree_row_reference_deleted" proxy path))

(define (gtk-tree-row-reference-inserted proxy path)
"  ARGS: 
     proxy  - object Object, 
     path  - struct TreePath
   RETURN: void
"
  (gi-function-invoke "Gtk-tree_row_reference_inserted" proxy path))

(define (gtk-tree-set-row-drag-data selection-data tree-model path)
"  ARGS: 
     selection-data  - struct SelectionData, 
     tree-model  - Unhandled argument type tag 16, 
     path  - struct TreePath
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-tree_set_row_drag_data" selection-data tree-model path))

(define (gtk-true)
"  ARGS: 
   RETURN: gboolean
"
  (gi-function-invoke "Gtk-true"))

