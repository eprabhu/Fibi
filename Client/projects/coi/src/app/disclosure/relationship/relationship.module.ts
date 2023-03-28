import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RelationshipComponent} from './relationship.component';
import {RouterModule, Routes} from "@angular/router";
import { DefineRelationComponent } from './define-relation/define-relation.component';
import { FormsModule } from '@angular/forms';

const routes: Routes = [{path: '', component: RelationshipComponent}];

@NgModule({
    declarations: [
        RelationshipComponent,
        DefineRelationComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        FormsModule
    ]
})
export class RelationshipModule {
}
