import io.quarkus.gradle.tasks.QuarkusNative
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "dit-calendar"
version = "0.6.0.0-SNAPSHOT"

plugins {
    val kotlinVersion = "1.3.72"

    kotlin("jvm") version kotlinVersion
    kotlin("plugin.serialization") version kotlinVersion
    id("org.jetbrains.kotlin.plugin.allopen") version kotlinVersion

    id("io.quarkus") version "1.3.2.Final"
}

repositories {
    mavenLocal()
    mavenCentral()
    maven { url = uri("https://jitpack.io") }
    maven { url = uri("https://kotlin.bintray.com/kotlinx") }
}

dependencies {
    val fuelVersion = "2.2.1"
    val kittinunfResultVersion = "3.0.0"
    val kotlinxSerializationVersion = "0.20.0"
    val camelQuarkusVersion = "1.0.0-M6"

    compileOnly(kotlin("reflect"))

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-runtime:$kotlinxSerializationVersion")

    implementation("com.github.kittinunf.fuel:fuel:$fuelVersion")
    implementation("com.github.kittinunf.result:result:$kittinunfResultVersion")

    implementation("org.apache.camel.quarkus:camel-quarkus-kotlin:$camelQuarkusVersion")
    implementation("org.apache.camel.quarkus:camel-quarkus-bean:$camelQuarkusVersion")
    implementation("org.apache.camel.quarkus:camel-quarkus-platform-http:$camelQuarkusVersion")
    implementation("org.apache.camel.quarkus:camel-quarkus-jackson:$camelQuarkusVersion")
    implementation("org.apache.camel.quarkus:camel-quarkus-log:$camelQuarkusVersion")
    implementation("org.apache.camel.quarkus:camel-quarkus-telegram:$camelQuarkusVersion")
    //implementation("org.apache.camel.quarkus:camel-quarkus-support-webhook:$camelQuarkusVersion")
    //implementation("org.apache.camel.quarkus:camel-quarkus-netty-http:$camelQuarkusVersion")
}

tasks {
    named<QuarkusNative>("buildNative") {
        isEnableHttpUrlHandler = true
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}

val compileKotlin: KotlinCompile by tasks
compileKotlin.kotlinOptions {
    jvmTarget = "1.8"
    freeCompilerArgs = listOf("-Xjsr305=strict")
}

val compileTestKotlin: KotlinCompile by tasks
compileTestKotlin.kotlinOptions {
    jvmTarget = "1.8"
}

allOpen {
    annotation("javax.ws.rs.Path")
    annotation("javax.enterprise.context.ApplicationScoped")
    annotation("io.quarkus.test.junit.QuarkusTest")
    //annotation("javax.enterprise.inject.Produces")
}