package com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement;

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
public class OPAStudentSubordinateInvolvementRequestModel extends ProgrammedElementRequestModel {

	private Integer opaStudSubInvId;
	private Integer opaDisclosureId;
	private String opaPersonTypeCode; 
	private String opaPersonType;
    private String personId;
    private String personName;
    private String natureOfWork;
    private String updateUser;
    private String relationWithPerson;
    private BigDecimal numOfDays;
    private Integer opaDisclPersonEntityId;
    private Integer personEntityId;
    private OPAPersonEntityInfoDTO entityInfo;
    private String description1;
    private String description2;

}