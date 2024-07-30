package com.polus.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "PERSON_LOGIN_DETAILS")
@Data
public class PersonLoginDetail implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "LOGIN_DETAIL_ID")
    private Integer loginDetailId;

    @Column(name = "PERSON_ID")
    private String personId;

    @Column(name = "FULL_NAME")
    private String fullName;

    @Column(name = "LOGIN_STATUS")
    private String loginStatus;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;
}
