package com.ditcalendar.bot

import com.ditcalendar.bot.config.*
import com.ditcalendar.bot.data.OnlyText
import com.ditcalendar.bot.data.TelegramLink
import com.ditcalendar.bot.data.WithInline
import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.error.InvalidRequest
import com.ditcalendar.bot.formatter.parseResponse
import com.ditcalendar.bot.service.DitCalendarService
import com.ditcalendar.bot.service.ServerDeploymentService
import com.ditcalendar.bot.service.assingAnnonCallbackCommand
import com.ditcalendar.bot.service.assingWithNameCallbackCommand
import com.elbekD.bot.Bot
import com.elbekD.bot.server
import com.elbekD.bot.types.InlineKeyboardButton
import com.elbekD.bot.types.InlineKeyboardMarkup
import com.elbekD.bot.types.Message
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.failure
import com.github.kittinunf.result.success
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch

val helpMessage =
        """
            Mögliche Befehle sind
            /postcalendar {Hier Id einfügen} = Postet den Calendar mit der angegebenen ID
            /help = Zeigt alle Befehle an
        """.trimIndent()

fun main(args: Array<String>) {


    val config by config()

    val token = config[telegram_token]
    val herokuApp = config[heroku_app_name]
    val calendarService = DitCalendarService()
    val serverDeploymentService = ServerDeploymentService()

    val bot = if (config[webhook_is_enabled]) {
        GlobalScope.launch {
            serverDeploymentService.deployServer()
        }
        Bot.createWebhook(config[bot_name], token) {
            url = "https://$herokuApp.herokuapp.com/$token"

            /*
            Jetty server is used to listen to incoming request from Telegram servers.
            */
            server {
                host = "0.0.0.0"
                port = config[server_port]
            }
        }
    } else Bot.createPolling(config[bot_name], token)

    bot.onCallbackQuery { callbackQuery ->
        serverDeploymentService.deployServer()
        val request = callbackQuery.data
        val originallyMessage = callbackQuery.message

        if (request == null || originallyMessage == null) {
            bot.answerCallbackQuery(callbackQuery.id, "fehlerhafte Anfrage")
        } else {
            val msgUser = callbackQuery.from
            val telegramLink = TelegramLink(originallyMessage.chat.id, msgUser.id, msgUser.username, msgUser.first_name)
            val response = calendarService.executeCallback(telegramLink, request)

            when (val result = parseResponse(response)) {
                is OnlyText -> {
                    response.failure { bot.answerCallbackQuery(callbackQuery.id, result.message) }
                    response.success {
                        bot.answerCallbackQuery(callbackQuery.id, "erfolgreich ausgetragen")
                        bot.editMessageText(originallyMessage.chat.id, originallyMessage.message_id, text = result.message,
                                parseMode = "MarkdownV2")
                    }
                }
                is WithInline -> {
                    bot.answerCallbackQuery(callbackQuery.id, result.callbackNotificationText)
                    val inlineButton = InlineKeyboardButton(result.callBackText, callback_data = result.callBackData)
                    val inlineKeyboardMarkup = InlineKeyboardMarkup(listOf(listOf(inlineButton)))
                    bot.editMessageText(originallyMessage.chat.id, originallyMessage.message_id, text = result.message,
                            parseMode = "MarkdownV2", disableWebPagePreview = true, markup = inlineKeyboardMarkup)
                }
            }
        }
    }

    //for deeplinking
    bot.onCommand("/start") { msg, opts ->
        serverDeploymentService.deployServer()
        val msgUser = msg.from
        //if message user is not set, we can't process
        if (msgUser == null) {
            bot.sendMessage(msg.chat.id, "fehlerhafte Anfrage")
        } else {
            if (opts != null && opts.startsWith("assign")) {

                val taskId: Long? = opts.substringAfter("_").toLongOrNull()
                if (taskId != null) {
                    val assignMeButton = InlineKeyboardButton("Mit Telegram Namen", callback_data = assingWithNameCallbackCommand + taskId)
                    val annonAssignMeButton = InlineKeyboardButton("Annonym", callback_data = assingAnnonCallbackCommand + taskId)
                    val inlineKeyboardMarkup = InlineKeyboardMarkup(listOf(listOf(assignMeButton, annonAssignMeButton)))
                    bot.sendMessage(msg.chat.id, "Darf ich dein Namen verwenden?", "MarkdownV2", true, markup = inlineKeyboardMarkup)
                } else {
                    sendMessage(Result.error(InvalidRequest()), bot, msg)
                }
            } else {
                bot.sendMessage(msg.chat.id, helpMessage)
            }
        }
    }

    bot.onCommand("/help") { msg, _ ->
        bot.sendMessage(msg.chat.id, helpMessage)
    }

    bot.onCommand("/postcalendar") { msg, opts ->
        serverDeploymentService.deployServer()
        val msgUser = msg.from
        //if message user is not set, we can't process
        if (msgUser == null) {
            bot.sendMessage(msg.chat.id, "fehlerhafte Anfrage")
        } else {
            val response = calendarService.executePublishCalendarCommand(opts)
            sendMessage(response, bot, msg)
        }
    }

    bot.start()
}

private fun sendMessage(response: Result<Base, Exception>, bot: Bot, msg: Message) {
    when (val result = parseResponse(response)) {
        is OnlyText ->
            bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true)
        is WithInline -> {
            val inlineButton = InlineKeyboardButton(result.callBackText, callback_data = result.callBackData)
            val inlineKeyboardMarkup = InlineKeyboardMarkup(listOf(listOf(inlineButton)))
            bot.sendMessage(msg.chat.id, result.message, "MarkdownV2", true, markup = inlineKeyboardMarkup)
        }
    }
}