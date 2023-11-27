package com.polus.fibicomp.coi.dto;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class CompleteReivewRequestDto {

	private Map<Integer,Integer> disclosureIdNumberMap;

}
