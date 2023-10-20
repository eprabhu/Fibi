package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.math.BigDecimal;

import com.polus.formbuilder.programmedelement.ProgrammedElementRequestModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class OPACompUncompRequestModel extends ProgrammedElementRequestModel{

	    private Integer opaDisclActivityId;
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
	    private String updateUser;
	    private OPAPersonEntityInfoDTO entityInfo;
}
