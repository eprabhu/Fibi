import { Component, OnInit } from '@angular/core';
import { SharedModule } from '../../shared/shared.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';
import { Create_Entity, DuplicateCheckObj, EntityDupCheckConfig } from '../shared/entity-interface';
import { ActivatedRoute, Router } from '@angular/router';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';
import { NavigationService } from "../../common/services/navigation.service";
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { deepCloneObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-create-entity',
    templateUrl: './create-entity.component.html',
    styleUrls: ['./create-entity.component.scss'],
    imports: [SharedModule, SharedEntityManagementModule, FormsModule, CommonModule],
    standalone: true
})
export class CreateEntityComponent implements OnInit {

    constructor(private _activatedRoute: ActivatedRoute,
        private _navigationService: NavigationService,
        private _router: Router,
        public _commonService: CommonService,
        private _informationAndHelpTextService: InformationAndHelpTextService) { }

    createEntityObj = new Create_Entity();
    $performAction = new Subject<'SAVE_AND_VALIDATE'|'VALIDATE_ONLY'>();
    dupCheckPayload: DuplicateCheckObj;
    entityDupCheckConfig = new EntityDupCheckConfig();
    ngOnInit() {
        this._informationAndHelpTextService.moduleConfiguration = this._commonService
            .getSectionCodeAsKeys(this._activatedRoute.snapshot.data.entityConfig);
        window.scroll(0, 0);
    }

    proceedCreateEntity() {
        this.$performAction.next('VALIDATE_ONLY');
    }

    goBack() {
        if (this._navigationService.previousURL) {
            this._router.navigateByUrl(this._navigationService.previousURL);
        } else {
            this._commonService.redirectionBasedOnRights();
        }
    }

    getMandatoryResponse(event: DuplicateCheckObj) {
        this.entityDupCheckConfig.duplicateView = 'MODAL_VIEW';
        this.dupCheckPayload = deepCloneObject(event);
    }

    duplicateCheckResponse(event: 'CLOSE_BTN' | 'SECONDARY_BTN' | 'PRIMARY_BTN' | 'NOT_FOUND') {
        if(event === 'PRIMARY_BTN' || event === 'NOT_FOUND') {
            this.$performAction.next('SAVE_AND_VALIDATE');
        }
    }
}
