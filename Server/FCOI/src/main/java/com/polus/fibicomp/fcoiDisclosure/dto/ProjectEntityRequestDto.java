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
    private Integer coiDisclProjectEntityRelId;
    private Integer coiDisclProjectId;
    private String projectConflictStatusCode;
    private Integer commentId;
    private String comment;
    private Integer personEntityId;
    private Boolean relationshipSFIMode;
    private Boolean applyAll;
}
