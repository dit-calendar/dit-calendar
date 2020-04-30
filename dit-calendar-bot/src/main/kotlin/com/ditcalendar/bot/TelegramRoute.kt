package com.ditcalendar.bot

import com.ditcalendar.bot.data.wrongRequestMessage
import com.ditcalendar.bot.service.TelegramService
import org.apache.camel.Exchange
import org.apache.camel.component.telegram.model.IncomingCallbackQuery
import org.apache.camel.component.telegram.model.IncomingMessage
import org.apache.camel.component.telegram.model.OutgoingCallbackQueryMessage
import org.apache.camel.quarkus.kotlin.routes
import org.eclipse.microprofile.config.inject.ConfigProperty
import javax.enterprise.context.ApplicationScoped
import javax.enterprise.inject.Produces


@ApplicationScoped
class TelegramRoute(val telegramService: TelegramService) {

    @ConfigProperty(name = "bot.name")
    lateinit var botName: String

    @ConfigProperty(name = "webhook.enabled")
    var webHookIsEnabled: Boolean = false

    @Produces
    fun myRoutes() = routes {

        //TODO configure webhook
        from("telegram:bots")
                .process { e: Exchange ->
                    val msg = e.`in`.body

                    if (msg is IncomingCallbackQuery) {
                        val request = msg.data
                        val originallyMessage = msg.message
                        val chatId = originallyMessage.chat.id
                        val msgUser = msg.from

                        val callbackResponse = if (request != null && originallyMessage != null) {
                            telegramService.callbackRequestParser(request, originallyMessage, msgUser)
                        } else {
                            OutgoingCallbackQueryMessage().apply { text = wrongRequestMessage }
                        }
                        callbackResponse.callbackQueryId = msg.id //TODO wrong id?
                        callbackResponse.chatId = chatId

                        e.message.body = callbackResponse

                    } else if (msg is IncomingMessage && msg.entities.isNotEmpty()
                            && msg.entities[0].type == "bot_command") {

                        val request = msg.text
                        val chatId = msg.chat.id.toLong()
                        val msgUser = msg.from

                        e.message.body = if (msgUser == null || request == null)
                            wrongRequestMessage
                        else
                            telegramService.commandRequestParser(request, chatId, msgUser)
                    }
                }
                .log("\${body}")
                //.bean(chatBot, "chatBotProcess({header.CamelTelegramChatId}, \${body})")
                .to("telegram:bots")
    }
}