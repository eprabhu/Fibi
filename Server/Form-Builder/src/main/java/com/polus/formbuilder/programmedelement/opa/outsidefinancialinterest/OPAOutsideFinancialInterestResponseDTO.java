package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.util.Date;

import com.polus.formbuilder.programmedelement.opa.common.OPAPersonEntityInfoDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPAOutsideFinancialInterestResponseDTO{
	
	private Integer opaOutsideFinancialInterestId;
    private Integer opaDisclosureId;
    private Integer opaDisclPersonEntityId;
    private Integer personEntityId;
	private String personsRelationWithEntity;   
    private String entityRelationWithInstitute;
    private String description1;
    private String description2;
    private Date updateTimestamp;
    private String updateUser;
    private OPAPersonEntityInfoDTO entityInfo;
    
}
