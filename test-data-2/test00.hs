todoItem ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem (TodoItemConfig initialValue) =
  elClass "div" "todo-item" $ mdo

    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      text initialValue

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed " <$ eRemove

    pure $
      TodoItem dComplete eRemove

