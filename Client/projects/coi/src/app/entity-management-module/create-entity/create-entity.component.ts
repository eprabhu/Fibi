import { Component } from '@angular/core';
import { SharedModule } from '../../shared/shared.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';
import { Create_Entity } from '../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { ActivatedRoute, Router } from '@angular/router';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';

@Component({
  selector: 'app-create-entity',
  templateUrl: './create-entity.component.html',
  styleUrls: ['./create-entity.component.scss'],
  imports: [SharedModule, SharedEntityManagementModule],
  standalone: true
})
export class CreateEntityComponent {

    constructor(private _activatedRoute: ActivatedRoute,
        public _commonService: CommonService, private _informationAndHelpTextService: InformationAndHelpTextService) {}

    createEntityObj: Create_Entity = new Create_Entity();
    saveEntity = new Subject();
    initalProceed = new Subject();

    ngOnInit() {
        this._informationAndHelpTextService.moduleConfiguration = this._commonService.getSectionCodeAsKeys(this._activatedRoute.snapshot.data.entityConfig);;
        window.scroll(0, 0);
    }

    saveBasicEntityDetails(event) {
        // if(event) {
        //     openModal('entityProceedCheckMatch');
        // }
    }

    createEntity() {
        // this.saveEntity.next(true);
        // hideModal('entityProceedCheckMatch');
    }

    proceedCreateEntity() {
        this.saveEntity.next(true);

        // this.initalProceed.next(true);
    }
}
