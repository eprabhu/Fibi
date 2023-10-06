package com.polus.formbuilder.model;

import java.util.List;

import com.polus.formbuilder.dto.FormResponseDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BlankFormResponse {

	private List<Integer> applicableFormsBuilderIds;
	
	private FormResponseDTO form;
	
}
