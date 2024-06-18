import { Component, OnInit, AfterViewInit } from '@angular/core';
import { FBConfiguration } from '../../../../shared/form-builder-view/form-builder-interface';
import { ActivatedRoute } from '@angular/router';
import { FormBuilderCreateService } from '../../form-builder-create.service';
import { CommonService } from '../../../../common/services/common.service';


@Component({
    selector: 'app-form-preview',
    templateUrl: './form-preview.component.html',
    styleUrls: ['./form-preview.component.scss']
})
export class FormPreviewComponent implements OnInit, AfterViewInit {

    fbConfiguration = new FBConfiguration();
    formBuilderId: any;

    constructor(private _route: ActivatedRoute,
        public _formBuilderService: FormBuilderCreateService,
        private _commonService: CommonService) { }

    ngOnInit() {
        this.formBuilderId = this._route.snapshot.queryParamMap.get('formBuilderId');
    }

    ngAfterViewInit(): void {
        if (this.formBuilderId) {
            this.fbConfiguration.moduleItemCode = '23';
            this.fbConfiguration.moduleSubItemCode = '0';
            this.fbConfiguration.documentOwnerPersonId = this._commonService.currentUserDetails.personId;
            this.fbConfiguration.formBuilderId = this.formBuilderId;
            this._formBuilderService.formBuilderEvents.next({ eventType: 'BLANK_FORM', data: this.fbConfiguration });
        }
    }

}
