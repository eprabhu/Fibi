package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

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
public class OPAInstituteResourceUseRequestModel extends ProgrammedElementRequestModel {

	private Integer opaInstResId;
	private Integer opaDisclosureId;
	private Integer opaDisclPersonEntityId; 
	private Integer personEntityId; 
    private String updateUser;
    private String description;
    private String description1;
    private String description2;
    private OPAPersonEntityInfoDTO entityInfo;

}