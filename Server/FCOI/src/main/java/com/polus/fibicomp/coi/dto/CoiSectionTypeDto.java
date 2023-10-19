package com.polus.fibicomp.coi.dto;

import java.util.List;

import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiSectionTypeDto {

	private List<PersonEntity> personEntities;

	private List<DisclosureDetailDto> projectList;

	private List<CoiSectionsType> coiSectionsTypeList;

	private QuestionnaireDataBus questionnaireDataBus;

}
