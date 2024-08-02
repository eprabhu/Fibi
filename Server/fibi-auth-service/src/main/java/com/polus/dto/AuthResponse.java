package com.polus.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuthResponse implements Serializable {

    private static final long serialVersionUID = 1L;

    private String personID;
    private String userName;
    private String firstName;
    private String lastName;
    private String fullName;
    private String unitNumber;
    private String unitName;
    private Boolean isUnitAdmin; //not using
    private Boolean login;
    private String userType;
    private String secretImageUri;
    private Boolean isExternalUser;
    private String gender;
    private String email;
    private String primaryTitle;

}
