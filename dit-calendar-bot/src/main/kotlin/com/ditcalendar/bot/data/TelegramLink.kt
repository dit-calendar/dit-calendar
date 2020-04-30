package com.ditcalendar.bot.data

import kotlinx.serialization.Serializable

@Serializable
data class TelegramLink(val chatId: Long,
                        val userId: Long,
                        val userName: String? = null,
                        val firstName: String?)