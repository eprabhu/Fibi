package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Date;

import jakarta.persistence.Entity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPACompUnCompResponseDTO{
	
	private Integer opaDiscActivityId;
    private Integer opaDisclosureId;
    private Integer opaDisclPersonEntityId;
    private Integer personEntityId;
	private String isCompensated;
    private BigDecimal numOfDaysSummer;
    private BigDecimal numOfDaysAcademic;
    private BigDecimal numOfDaysInYear;
    private String natureOfWork;
    private String description1;
    private String description2;
    private Date updateTimestamp;
    private String updateUser;
    private OPAPersonEntityInfoDTO entityInfo;
    
}
