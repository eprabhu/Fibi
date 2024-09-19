package com.polus.fibicomp.coi.hierarchy.dto;

import java.util.List;

import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class ProjectPersonDto {

	private String keyPersonId;

	private String keyPersonName;
	
	private String keyPersonRole;
	
	private String homeUnitName;
	
	private String homeUnitNumber;

	private List<DisclosureDto> disclosures;

}
