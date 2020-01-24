package com.ditcalendar.bot.data.core

import kotlinx.serialization.Serializable

@Serializable
abstract class Base {
    var entryId: Long = 0

    var version: Int = 0

    abstract fun toStringInMarkdown(): String
}