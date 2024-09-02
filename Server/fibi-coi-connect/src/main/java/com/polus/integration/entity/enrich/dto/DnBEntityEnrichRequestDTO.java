package com.polus.integration.entity.enrich.dto;

import java.util.List;

import lombok.Data;

@Data
public class DnBEntityEnrichRequestDTO {
	
	private String duns;
	
	private Integer entityId;
	
	private List<String> datablock;

}