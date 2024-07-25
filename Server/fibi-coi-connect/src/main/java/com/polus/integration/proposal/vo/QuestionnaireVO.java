package com.polus.integration.proposal.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class QuestionnaireVO {

	private Integer proposalNumber;

	private String personId;

	private Integer questionnaireId;

	private Integer questionId;

	private String answer;

	private String personHomeUnit;

	private String coiProjectTypeCode;

	private String attribute1Label;

	private String attribute1Value;

	private String attribute2Label;

	private String attribute2Value;

	private String attribute3Label;

	private String attribute3Value;

	private transient Integer disclosureId;

}
