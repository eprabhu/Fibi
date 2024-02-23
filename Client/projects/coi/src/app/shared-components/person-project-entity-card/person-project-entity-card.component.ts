import { Component, Input, OnChanges } from '@angular/core';
import { PersonProjectOrEntity } from '../shared-interface';
import { CommonService } from '../../common/services/common.service';
import { getPersonLeadUnitDetails } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-person-project-entity-card',
    templateUrl: './person-project-entity-card.component.html',
    styleUrls: ['./person-project-entity-card.component.scss']
})

export class PersonProjectEntityCardComponent implements OnChanges {

    @Input() personProjectOrEntity: PersonProjectOrEntity = new PersonProjectOrEntity();
    @Input() disclosureHeaderDetails;

    isReadMore = false;
    personUnitDetail = '';

    constructor(public _commonService: CommonService) { }

    ngOnChanges(): void {
        this.personUnitDetail = this.getPersonLeadUnitDetail();
    }
    
    getPersonLeadUnitDetail() {
        return getPersonLeadUnitDetails(this.personProjectOrEntity);
    }

    setProjectAndNumberForModuleCodes(): any {
        return this.personProjectOrEntity.projectDetails['moduleCode'] == 3 ?
               this.personProjectOrEntity.projectDetails['moduleItemId'] : this.personProjectOrEntity.projectDetails['moduleItemKey'];
    }

}
