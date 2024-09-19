package com.polus.fibicomp.coi.hierarchy.dto;

import java.sql.Timestamp;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class DisclosureDto {

	private Integer disclosureId;
    
	private String disclosureType;
    
	private String disclosureStatus;

	private String dispositionStatus;
    
	private String reviewStatus;
    
	private Timestamp certificationDate;

}
