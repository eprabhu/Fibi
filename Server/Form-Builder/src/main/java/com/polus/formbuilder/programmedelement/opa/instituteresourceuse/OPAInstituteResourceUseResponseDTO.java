package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

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
public class OPAInstituteResourceUseResponseDTO {

	private Integer opaInstResId;
    private Integer opaDisclosureId;
    private Date updateTimestamp;
    private Integer opaDisclPersonEntityId;
    private Integer personEntityId;
    private String updateUser;
    private String description;
    private String description1;
    private String description2;
    private OPAPersonEntityInfoDTO entityInfo;

}
