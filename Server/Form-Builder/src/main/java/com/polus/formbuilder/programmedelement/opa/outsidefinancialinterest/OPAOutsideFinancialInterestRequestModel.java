package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.math.BigDecimal;

import com.polus.formbuilder.programmedelement.ProgrammedElementRequestModel;
import com.polus.formbuilder.programmedelement.opa.common.OPAPersonEntityInfoDTO;

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
public class OPAOutsideFinancialInterestRequestModel extends ProgrammedElementRequestModel{

	    private Integer opaOutsideFinancialInterestId;
	    private Integer opaDisclosureId;
	    private Integer opaDisclPersonEntityId;
	    private Integer personEntityId;	  
	    private String personsRelationWithEntity;	   
	    private String entityRelationWithInstitute;
	    private String description1;
	    private String description2;
	    private String updateUser;
	    private OPAPersonEntityInfoDTO entityInfo;
}
