package com.polus.fibicomp.fcoiDisclosure.dto;


import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProjectEntityRequestDto {

    private Integer disclosureId;
    private Integer disclosureNumber;
    private String personId;
}
