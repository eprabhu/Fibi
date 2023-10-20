package com.polus.fibicomp.opa.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

@Getter
@Setter
@Builder
public class OPADashboardDto {

    private Integer opaDisclosureId;
    private String opaDisclosureNumber;
    private Integer opaCycleNumber;
    private Date periodStartDate;
    private Date periodEndDate;
    private Date openDate;
    private Date closeDate;
    private Boolean opaCycleStatus;
    private String personName;
    private String homeUnit;
    private String homeUnitName;
    private Boolean isFaculty;
    private Boolean isFallSabatical;
    private Boolean isSpringSabatical;
    private Boolean isFullTime;
    private BigDecimal summerCompMonths;
    private Boolean receivedSummerComp;
    private String dispositionStatusCode;
    private String opaDisclosureStatusCode;
    private Timestamp submissionTimestamp;
    private String certifiedBy;
    private String certificationText;
    private Boolean hasPotentialConflict;
    private String conflictDescription;
    private String createUser;
    private Timestamp createTimestamp;
    private String adminPersonId;
    private String adminPersonName;
    private Integer adminGroupId;
    private Integer adminGroupName;
    private String updateUser;
    private Timestamp updateTimestamp;
    private String updateUserFullName;
    private String dispositionStatus;
    private String disclosureStatus;
}
