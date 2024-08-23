import { Component } from '@angular/core';
import { SharedModule } from '../../shared/shared.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';
import { Create_Entity } from '../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Router } from '@angular/router';
import { Subject } from 'rxjs';

@Component({
  selector: 'app-create-entity',
  templateUrl: './create-entity.component.html',
  styleUrls: ['./create-entity.component.scss'],
  imports: [SharedModule, SharedEntityManagementModule],
  standalone: true
})
export class CreateEntityComponent {

    constructor() {}

    createEntityObj: Create_Entity = new Create_Entity();
    saveEntity = new Subject();
    initalProceed = new Subject();

    ngOnInit() {
        window.scroll(0, 0);
    }

    saveBasicEntityDetails(event) {
        if(event) {
            // openModal('entityProceedCheckMatch');
        }
    }

    // createEntity() {
    //     this.saveEntity.next(true);
    //     hideModal('entityProceedCheckMatch');
    // }

    proceedCreateEntity() {
        this.initalProceed.next(true);
    }
}
