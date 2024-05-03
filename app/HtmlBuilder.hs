module HtmlBuilder (
  createUserConnectedMsg,
  createUserChatMsg,
  createErrorMsg,
) where

import Data.Text (Text, pack)
import Lucid
import Lucid.Htmx (hxSwapOob_, hxSwap_, hxWs_)

createUserConnectedHtml :: Text -> Int -> Html ()
createUserConnectedHtml userName numClients = do
  div_
    [id_ "chat-room", hxSwap_ "#chat-room"]
    ( do
        div_
          [class_ "welcome-panel"]
          ( do
              p_
                [id_ "welcome-panel"]
                ( "Welcome, "
                    <> toHtml userName
                    <> "! Users online: "
                    <> toHtml (show numClients)
                )
          )
        div_ [id_ "chat-room-content"] mempty
        form_
          [class_ "chat-room form", hxWs_ "send:submit"]
          ( do
              input_ [type_ "text", class_ "form-control", name_ "chatMessage"]
              button_
                [ type_ "submit"
                , class_ "btn btn-primary form-control mt-2 py-2"
                ]
                "Send Message"
          )
    )

createErrorMsg :: Text -> Text
createErrorMsg errorMsg =
  htmlToText $ createErrorMsgHtml errorMsg
 where
  createErrorMsgHtml :: Text -> Html ()
  createErrorMsgHtml msg = do
    div_
      [id_ "error-panel", hxSwap_ "#error-panel", class_ "alert alert-danger visible"]
      ( do
          i_ (toHtml msg)
      )

createUserConnectedMsg :: Text -> Int -> Text
createUserConnectedMsg userName numClients =
  htmlToText $ createUserConnectedHtml userName numClients

createUserChatMsg :: Text -> Text -> Text
createUserChatMsg userName chatMessage =
  htmlToText $ createUserChatMessageHtml userName chatMessage

createUserChatMessageHtml :: Text -> Text -> Html ()
createUserChatMessageHtml "_system_" chatMessage = do
  div_
    [ hxSwapOob_ "beforeend:#chat-room-content"
    , class_ "chat-message"
    ]
    ( do
        p_ [style_ "font-weight: bold; color: orange"] (toHtml chatMessage)
    )
createUserChatMessageHtml userName chatMessage = do
  div_
    [ hxSwapOob_ "beforeend:#chat-room-content"
    , class_ "chat-message"
    ]
    ( do
        p_ (toHtml userName <> " : " <> toHtml chatMessage)
    )

htmlToText :: Html () -> Text
htmlToText = pack . show
