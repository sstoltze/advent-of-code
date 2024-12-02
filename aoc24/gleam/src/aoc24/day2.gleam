import gleam/int
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/string

type Report =
  List(Int)

fn increasing_or_decreasing(report: Report) -> Bool {
  report == list.sort(report, int.compare)
  || report == list.sort(report, order.reverse(int.compare))
}

fn parse_report(row: String) -> List(Int) {
  row
  |> string.split(" ")
  |> list.map(int.parse)
  |> result.values()
}

fn by_at_most_3(report: Report) -> Bool {
  let assert [first, ..rest] = report
  list.fold_until(rest, #(True, first), fn(acc, next) {
    let #(_result, prev) = acc
    case int.absolute_value(next - prev) {
      n if 1 <= n && n <= 3 -> list.Continue(#(True, next))
      _ -> list.Stop(#(False, next))
    }
  })
  |> pair.first()
}

fn safe_by_removing_one(report: Report) -> Bool {
  let reports = list.combinations(report, list.length(report) - 1)
  list.any(reports, safe)
}

fn safe(report: Report) -> Bool {
  increasing_or_decreasing(report) && by_at_most_3(report)
}

pub fn run(input: List(String)) -> #(Int, Int) {
  let reports = list.map(input, parse_report)
  let safe_with_dampener = list.filter(reports, safe_by_removing_one)
  let safe_reports = list.filter(safe_with_dampener, safe)
  #(list.length(safe_reports), list.length(safe_with_dampener))
}
