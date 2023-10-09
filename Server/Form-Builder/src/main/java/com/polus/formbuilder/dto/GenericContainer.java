package com.polus.formbuilder.dto;

public class GenericContainer<T> {
    private T value;

    public GenericContainer(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }
}