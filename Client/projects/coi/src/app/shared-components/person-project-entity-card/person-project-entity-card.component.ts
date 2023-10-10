import { Component, Input } from '@angular/core';
import { PersonProjectOrEntity } from '../shared-interface';

@Component({
    selector: 'app-person-project-entity-card',
    templateUrl: './person-project-entity-card.component.html',
    styleUrls: ['./person-project-entity-card.component.scss']
})

export class PersonProjectEntityCardComponent {

    @Input() personProjectOrEntity: PersonProjectOrEntity = new PersonProjectOrEntity();

    isReadMore = false;

    setProjectAndNumberForModuleCodes(): any {
        return this.personProjectOrEntity.projectDetails['moduleCode'] == 3 ?
               this.personProjectOrEntity.projectDetails['moduleItemId'] : this.personProjectOrEntity.projectDetails['moduleItemKey'];
    }

}
