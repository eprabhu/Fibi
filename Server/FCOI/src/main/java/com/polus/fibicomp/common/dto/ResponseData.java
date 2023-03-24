package com.polus.fibicomp.common.dto;

import java.io.Serializable;
import java.sql.Timestamp;

/**
 * ResponseData
 *
 * @createdBy Ajin
 * @date 24 Nov 2021
 */
public class ResponseData implements Serializable {

    private Object data;
    private String message;
    private Timestamp timestamp;
    private boolean status;
    private String error;

    public ResponseData() {
    }

    public ResponseData(Object data, String message, Timestamp timestamp, boolean status) {
        this.data = data;
        this.message = message;
        this.timestamp = timestamp;
        this.status = status;
    }

    public ResponseData(Object data, String message, Timestamp timestamp, boolean status, String error) {
        this.data = data;
        this.message = message;
        this.timestamp = timestamp;
        this.status = status;
        this.error = error;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public boolean isStatus() {
        return status;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }
}
