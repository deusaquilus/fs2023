package org.deusaquilus.part2.pattern1_adapter;

import com.google.gson.*;

public class JavaExamplePure {
    record UserId(String value) {}
    record Person(UserId id, String firstName, String lastName) {}

public static class UserIdAdapter implements JsonSerializer<UserId> {
    @Override
    public JsonElement serialize(
            UserId src,
            java.lang.reflect.Type typeOfSrc,
            JsonSerializationContext context) {
        return new JsonPrimitive(src.value);
    }
}

    public static void main(String[] args) {
        Gson gson = new GsonBuilder()
            .registerTypeAdapter(UserId.class, new UserIdAdapter())
            .create();
        UserId id = new UserId("123");
        Person person = new Person(id, "John", "Doe");
        System.out.println(gson.toJson(person));
    }

}
