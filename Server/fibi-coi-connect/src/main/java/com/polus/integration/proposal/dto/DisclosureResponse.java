package com.polus.integration.proposal.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DisclosureResponse {

	private String disclosureStatus;
	private Integer disclosureId;
	private Integer disclosureSubmitted;
	private String message;
	private String error;

}
