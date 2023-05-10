import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RelationshipComponent} from './relationship.component';
import {RouterModule, Routes} from "@angular/router";
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from "@angular/material/icon";
import { DefineRelationComponent } from './define-relation/define-relation.component';
import { RelationshipService } from './relationship.service';
import {MatSnackBarModule} from '@angular/material/snack-bar';
import { FormsModule } from '@angular/forms';
import { SearchFilterPipe } from './directives/search-filter.pipe';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedModule } from '../../shared/shared.module';

const routes: Routes = [{path: '', component: RelationshipComponent}];

@NgModule({
    declarations: [
        RelationshipComponent,
        DefineRelationComponent,
        SearchFilterPipe
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        FormsModule,
        MatIconModule,
        MatInputModule,
        MatFormFieldModule,
        MatSnackBarModule,
        SharedComponentModule,
        SharedModule,
    ],
    providers: [
        RelationshipService
    ]
})
export class RelationshipModule {
}
