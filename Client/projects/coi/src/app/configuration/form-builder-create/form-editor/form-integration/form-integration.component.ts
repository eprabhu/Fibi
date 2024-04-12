import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
    selector: 'app-form-integration',
    templateUrl: './form-integration.component.html',
    styleUrls: ['./form-integration.component.scss']
})
export class FormIntegrationComponent implements OnInit {
    formTitle: string;
    
    constructor(private _route: ActivatedRoute) { }

    ngOnInit() {
        this._route.queryParamMap.subscribe(queryParams => {
            this.formTitle = queryParams.get('title');
        })

    }

}
