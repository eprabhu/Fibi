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

    private String personId;
    private String userName;
    private String fullName;
    private String homeUnit;
    private String gender;
    private String primaryTitle;
    private String homeUnitName;
    private Boolean isFaculty;
    
}
