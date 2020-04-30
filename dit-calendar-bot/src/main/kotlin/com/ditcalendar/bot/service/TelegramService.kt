package com.ditcalendar.bot.service

import com.ditcalendar.bot.data.*
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.formatter.StringFormatter
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.failure
import com.github.kittinunf.result.success
import org.apache.camel.CamelContext
import org.apache.camel.component.telegram.TelegramParseMode
import org.apache.camel.component.telegram.model.*
import javax.enterprise.context.ApplicationScoped

@ApplicationScoped
class TelegramService(private val ditCalendarService: DitCalendarService,
                      private val stringFormatter: StringFormatter,
                      private val context: CamelContext) {

    fun commandRequestParser(request: String, chatId: Long, msgUser: User): Any? =
            when {
                request.startsWith(helpCommand) -> helpMessage

                request.startsWith(postCalendarCommand) -> {
                    val opts = request.removePrefix("$postCalendarCommand ")
                    val response = ditCalendarService.executePublishCalendarCommand(opts)
                    buildCommandRespondMessage(response)
                }

                //for deeplinking
                request.startsWith(startCommand) -> {
                    if (request.startsWith(assignCommand)) {
                        val opts = request.removePrefix(assignCommand)
                        val telegramLink = TelegramLink(chatId, msgUser.id, msgUser.username, msgUser.firstName)
                        val response = ditCalendarService.executeTaskAssignmentCommand(telegramLink, opts)

                        buildCommandRespondMessage(response)
                    } else {
                        helpMessage
                    }
                }
                else -> null
            }

    fun callbackRequestParser(request: String, originallyMessage: IncomingMessage, msgUser: User): OutgoingCallbackQueryMessage {
        val callbackRequest = parse(request)
        val telegramLink = TelegramLink(originallyMessage.chat.id.toLong(),
                msgUser.id, msgUser.username, msgUser.firstName)
        val response = ditCalendarService.executeCallback(telegramLink, callbackRequest)
        return buildCallBackResponseMessage(response, originallyMessage)
    }

    private fun editOriginalMessage(originallyMessage: IncomingMessage, newText: String, replyMarkupMessage: InlineKeyboardMarkup?) {
        if(originallyMessage.text != newText || replyMarkupMessage != originallyMessage.replyMarkup) {
            val template = context.createProducerTemplate()
            val msg = EditMessageTextMessage(originallyMessage.chat.id,
                    originallyMessage.messageId.toInt(),
                    null,
                    newText,
                    "MarkdownV2",
                    true,
                    replyMarkupMessage)
            val msgResult = template.requestBody("telegram:bots", msg, MessageResult::class.java)
            println(msgResult)
        }
    }

    private fun buildCallBackResponseMessage(response: Result<Base, Exception>, originallyMessage: IncomingMessage): OutgoingCallbackQueryMessage {
        val callbackResponse = OutgoingCallbackQueryMessage()

        when (val result = stringFormatter.parseResponse(response)) {
            is OnlyText -> {
                response.failure { callbackResponse.text = result.message }
                response.success {
                    editOriginalMessage(originallyMessage, result.message, null)

                    callbackResponse.text = successfullyUnassignedMessage
                }
            }
            is WithInline -> {
                val inlineButton = InlineKeyboardButton.builder()
                        .text(result.callBackText).callbackData(result.callBackData).build()
                val inlineKeyboardMarkup = InlineKeyboardMarkup.builder()
                        .addRow(listOf(inlineButton))
                        .build()
                editOriginalMessage(originallyMessage, result.message, inlineKeyboardMarkup)

                callbackResponse.text = "calendar wurde neugeladen"
            }
        }
        return callbackResponse
    }

    private fun buildCommandRespondMessage(response: Result<Base, Exception>): OutgoingTextMessage {
        val msg = OutgoingTextMessage()
        msg.disableWebPagePreview = true
        msg.parseMode = TelegramParseMode.MARKDOWN.code

        when (val result = stringFormatter.parseResponse(response)) {
            is OnlyText ->
                msg.text = result.message
            is WithInline -> {
                val inlineButton = InlineKeyboardButton.builder()
                        .text(result.callBackText).callbackData(result.callBackData).build()

                val inlineKeyboardMarkup = InlineKeyboardMarkup.builder()
                        .addRow(listOf(inlineButton))
                        .build()

                msg.replyMarkup = inlineKeyboardMarkup
                msg.text = result.message
            }
        }
        return msg
    }

    companion object {
        private const val startCommand = "/start"
        private const val assignCommand = "$startCommand assign_"
        private const val helpCommand = "/help"
        private const val postCalendarCommand = "/postcalendar"
    }
}