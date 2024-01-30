package com.polus.fibicomp.coi.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class COIValidateDataDto {

    private String VALIDATION_TYPE;
    private String VALIDATION_MSG_TYPE;
    private String MESSAGE;
    private String SFIs;
    private String PROJ_SFI_DETAILS;

}
