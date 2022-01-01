## Функция расчета показателя и создания итоговой таблицы
## Принимает на вход файл .csv со всеми результатами ВПР, ОГЭ и ЕГЭ за требуемый год
## Шаблон .csv файла размещен отдельно в репозитории
## Изменять имена столбцов нельзя!

compute_equality <- function(dataset) {

    ### Преобразовать столбцы "Школа" и "Процедура" в тип "Фактор"
    dataset$school <- as.factor(dataset$school)
    dataset$oko <- as.factor(dataset$oko)

    ### Создать объекты типа "Лист" для последующего использования в циклах (школы и типы процедур)
    schools <- as.list(levels(dataset$school))
    oko_tests <- as.list(levels(dataset$oko))

    ### Подготовить пустой датасет, к которому будут присоединяться строки
    eq_data <- data.frame(school = character(), eq_vpr = numeric(), eq_oge = numeric(), eq_ege = numeric(), stringsAsFactors = F)

    ### Для каждой школы
    for (n in schools) { 
    
        ### Подготовка пустого вектора с названием текущей школы
        current_eq <- c(n, 0, 0, 0) 
        counter <- 2
        
        ### Для каждой оценочной процедуры
        for (j in oko_tests) { 
        
            ### Сделать выборку результатов по текущей школе и текущей оценочной процедуре
            sample_subset <- dataset[dataset$school == n & dataset$oko == j, ]
            
            ### Найти в выборке средние баллы, определяющие первый и третий квартиль результатов
            q1 <- quantile(sample_subset$avg_result)[2]
            q3 <- quantile(sample_subset$avg_result)[4]

            ### Высчитать количество результатов в первом и третьем квартиле, сохранить общее количество результатов
            weak_results_count <- nrow(sample_subset[sample_subset$avg_result <= q1, ])
            strong_results_count <- nrow(sample_subset[sample_subset$avg_result >= q3, ])
            total_results <- nrow(sample_subset)

            ### Подставить значения в формулу расчета показателя (доля низких результатов от доли высоких результатов)
            precise_eq <- (weak_results_count / total_results * 100) / (strong_results_count / total_results * 100) * 100
            eq <- round(precise_eq, digits = 2)

            ### Внести полученное значение по текущей оценочной процедуре в пустой вектор на соответствующее место
            current_eq[counter] <- as.numeric(eq)
            counter <- counter + 1
        }
        
        ### Добавить строку по текущей школе к датасету
        eq_data <- rbind(eq_data, current_eq)
    }

    ### Привести в порядок итоговый датасет (названия столбцов, типы переменных)
    colnames(eq_data) <- c("Школа", "П_ВПР", "П_ЕГЭ", "П_ОГЭ")
    eq_data$Школа <- as.factor(eq_data$Школа)
    eq_data[, c(2, 3, 4)] <- apply(eq_data[, c(2, 3, 4)], 2, function(x) as.numeric(x))

    ### Рассчитать итоговое значение показателя для каждой строки (школы)
    eq_data$Равенство <- round((eq_data$П_ВПР + eq_data$П_ЕГЭ + eq_data$П_ОГЭ) / 3, 2)

    return(eq_data)
}
                                   
## Обработать исходный датафрейм
oko_data <- read.csv("template.csv")

## Сохранить датасет со значениями показателя для каждой образовательной организации
equality <- compute_equality(oko_data)
write.csv2(equality, "equality_results.csv", row.names = FALSE)
