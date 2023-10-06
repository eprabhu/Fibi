package com.polus.formbuilder.model;

import com.polus.formbuilder.dto.FormResponseDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FormResponse {

	private FormResponseDTO form;
	
}
