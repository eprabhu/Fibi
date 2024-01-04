package com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement;

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
public class OPAStudentSubordinateInvolvementResponseDTO {

	private Integer opaStudSubInvId;
    private Integer opaDisclosureId;
	private String opaPersonTypeCode; 
	private String opaPersonType;
    private String personId;
    private String personName;
    private String natureOfWork;
    private String relationWithPerson;
    private Integer opaDisclPersonEntityId;
    private Date updateTimestamp;
    private String updateUser;
    private String description1;
    private String description2;
    private Integer personEntityId;
    private OPAPersonEntityInfoDTO entityInfo;

}
