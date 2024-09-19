package com.polus.fibicomp.globalentity.dto;

import com.polus.core.pojo.SponsorType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SponsorDetailsResponseDTO {

	private Integer entityId;
	private Integer id;
	private String sponsorCode;
	private String acronym;
	private SponsorType sponsorType;

}
