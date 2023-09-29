import { Component, Input, OnChanges } from '@angular/core';
import { PersonProjectOrEntity } from '../shared-interface';

@Component({
    selector: 'app-person-project-entity-card',
    templateUrl: './person-project-entity-card.component.html',
    styleUrls: ['./person-project-entity-card.component.scss']
})

export class PersonProjectEntityCardComponent implements OnChanges {

    @Input() personProjectOrEntity: PersonProjectOrEntity = new PersonProjectOrEntity();

    isReadMore = false;
    projectId: any;

    ngOnChanges() {
        this.projectId = this.setProjectAndNumberForModuleCodes();
    }

    setProjectAndNumberForModuleCodes(): any {
        return this.personProjectOrEntity.projectDetails['moduleCode'] == 3 ?
               this.personProjectOrEntity.projectDetails['moduleItemId'] : this.personProjectOrEntity.projectDetails['moduleItemKey'];
    }

}
