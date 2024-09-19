export class ProjectHierarchySliderPayload {
    isOpenSlider: boolean = false;
    projectNumber: string = null;
    projectTypeCode: string | number = null;
}

export class HierarchyProjectTree {
    projectIcon: string = '';
    projectType: string = '';
    projectNumber: string = '';
    projectTypeCode: string = '';
    linkedModule: HierarchyProjectTree[] = []; // Recursive relationship for nested modules
}

export class HierarchyProjectDetails {
    projectNumber?: string = null;
    sponsorCode?: string = null;
    primeSponsorCode?: string = null;
    sponsorName?: string = null;
    homeUnitName?: string = null;
    homeUnitNumber?: string = null;
    primeSponsorName?: string = null;
    projectStatus?: string = null;
    piName?: string = null;
    projectStartDate?: number = null;
    projectEndDate?: number = null;
    projectBadgeColour?: string = null;
    projectIcon?: string = null;
    projectType?: string = null;
    projectTypeCode?: string = null;
    projectTitle?: string = null;
    projectPersons?: ProjectKeyPerson[] = [];
}

export class ProjectKeyPerson {
    keyPersonId?: string = null;
    keyPersonName?: string = null;
    keyPersonRole?: string = null;
    homeUnitName?: string = null;
    homeUnitNumber?: string = null;
    disclosures?: HierarchyProjectDisclosure[] = [];
}

export class HierarchyProjectDisclosure {
    disclosureId?: number = null;
    reviewStatus?: string = null;
    disclosureType?: string = null;
    disclosureStatus?: string = null;
    dispositionStatus?: string = null;
    certificationDate?: number = null;
}
