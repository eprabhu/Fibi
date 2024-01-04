export class OPAInstituteResources {
    opaInstResId: number;
    opaDisclosureId: number;
    updateTimestamp: string;
    opaDisclPersonEntityId: number;
    personEntityId: number;
    updateUser: string;
    description: any;
    description1: any;
    description2: any;
    entityInfo = new EntityInfo;
    actionType: string;
}
  
  export class OPAInstituteResourcesPE {
    actionType: string;
    data: Array<OPAInstituteResources>;
  }
  
  export class EntityInfo {
    opaDisclPersonEntityId: number;
    personEntityId: number;
    personId: string;
    entityNumber: number;
    entityName: string;
    entityType: string;
    entityStatus: string;
    entityRiskCategory: string;
    countryName: string;
    relationship: string;
    isRelationshipActive: string | boolean;
    sfiVersionStatus: string;
    involvementStartDate: string;
    involvementEndDate: any;
  }

  export class EntitySaveRO {
    entityId: number;
    entityNumber: number;
    sponsorsResearch: boolean;
    involvementStartDate: string;
    staffInvolvement: string;
    studentInvolvement: string;
    instituteResourceInvolvement: string;
    validPersonEntityRelTypeCodes: number[];
}

export class RelationShipSaveRO {
    personEntityId: string;
    validPersonEntityRelTypeCodes: number[];
}
