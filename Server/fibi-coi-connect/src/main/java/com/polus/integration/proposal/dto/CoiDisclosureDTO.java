package com.polus.integration.proposal.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CoiDisclosureDTO {

	private Integer disclosureId;

	private Integer disclosureNumber;

	private Integer versionNumber;

	private String versionStatus;

	private String fcoiTypeCode;

	private String conflictStatusCode;

	private String dispositionStatusCode;

	private String reviewStatusCode;

	private String homeUnit;

	private String coiProjectTypeCode;

	private Integer moduleItemKey;

	private String personId;
}
