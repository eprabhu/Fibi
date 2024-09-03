package com.polus.fibicomp.globalentity.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SponsorRequestDTO {

	private Integer id;
	private Integer entityId;
	private String acronym;
	private String sponsorTypeCode;
	private String feedStatusCode;
	

}
